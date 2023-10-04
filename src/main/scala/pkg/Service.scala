package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import org.slf4j.{Logger, LoggerFactory}
import pkg.CalcParser.{parseExpression, Expr}
import pkg.Model.{DbItemParsed, DbKey, DbValue}
import pkg.StringUtils.{normalizeFormula, normalizeId}

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

class Service(db: Option[Db]) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()

  db.foreach { db =>
    // Restore the state from the database
    loadDbItems(db) match {
      case Success(sheetItems) =>
        val sheetItemsMap: Map[String, mutable.ArrayDeque[DbItemParsed]] = sheetItems.groupMap(_._1)(_._2)
        sheetItemsMap.foreach { case (sheetId, items) =>
          sheets.put(sheetId, new SheetImpl(sheetId, Some(db)))
          val itemsMap: Map[String, DbItemParsed] = items.groupMapReduce(_.cellId)(identity)((a, _) => a)
          val sorted                              = allItemsTopologicallySorted(itemsMap)
          sorted.reverseIterator.foreach { item =>
            if (putCell(sheetId, item.cellId, item.value).isEmpty) {
              logger.warn(s"Couldn't restore a value for cell ${item.cellId}")
            }
          }
        }
      case Failure(e) =>
        logger.warn("Couldn't load data from DB")
    }
  }

  private def loadDbItems(db: Db): Try[mutable.ArrayDeque[(String, DbItemParsed)]] = {
    Using(db.db.newIterator()) { iterator =>
      iterator.seekToFirst()
      val sheetItems: mutable.ArrayDeque[(String, DbItemParsed)] = mutable.ArrayDeque()
      while (iterator.isValid) {
        val key     = new String(iterator.key, UTF_8)
        val dbKey   = readFromString[DbKey](key)
        val value   = new String(iterator.value, UTF_8)
        val dbValue = readFromString[DbValue](value)
        val expression: Option[Expr] = if (dbValue.value.startsWith("=")) {
          normalizeFormula(dbValue.value.drop(1)).flatMap(formula =>
            parseExpression(cellId = dbKey.cellId, formula = formula)
          )
        } else None
        val referencedVariables: Seq[String] =
          expression.map(CalcParser.referencedVariables).map(_.toSeq).getOrElse(Seq.empty)
        val dbItemParsed = DbItemParsed(
          cellId = dbKey.cellId,
          value = dbValue.value,
          referencedVariables,
          traversed = false
        )
        sheetItems.append((dbKey.sheetId, dbItemParsed))
        iterator.next()
      }
      sheetItems
    }
  }

  private def allItemsTopologicallySorted(
      itemsMap: Map[String, DbItemParsed]
  ): mutable.ArrayDeque[DbItemParsed] = {
    val sorted: mutable.ArrayDeque[DbItemParsed]            = mutable.ArrayDeque()
    val buffer: mutable.ArrayDeque[(DbItemParsed, Boolean)] = mutable.ArrayDeque.from(itemsMap.values.map(_ -> false))

    while (buffer.nonEmpty) {
      val (item: DbItemParsed, traversed: Boolean) = buffer.removeLast()
      if (traversed) {
        sorted.prepend(item)
      } else {
        if (!item.traversed) {
          item.traversed = true
          buffer.append((item, true))
          item.referencedVariables.foreach { item2name =>
            val item2 = itemsMap(item2name)
            if (!item2.traversed) {
              buffer.append((item2, false))
            }
          }
        }
      }
    }
    sorted.foreach(_.traversed = false)
    sorted
  }

  def putCell(sheetId: String, cellId: String, sourceValue: String): Option[Either[String, Double]] = {
    (normalizeId(sheetId), normalizeId(cellId)) match {
      case (Some(sheetId), Some(cellId)) =>
        val sheet = sheets.computeIfAbsent(sheetId, sheetId => new SheetImpl(sheetId, db))
        sheet.putCellValue(id = cellId, sourceValue = sourceValue)
      case _ =>
        None
    }
  }

  def getCell(sheetId: String, cellId: String): Option[(String, Either[String, Double])] =
    (normalizeId(sheetId), normalizeId(cellId)) match {
      case (Some(sheetId), Some(cellId)) =>
        Option(sheets.get(sheetId)).flatMap(_.getCellValue(cellId))
      case _ =>
        None
    }

  def getSheet(sheetId: String): Option[Map[String, (String, Either[String, Double])]] =
    normalizeId(sheetId).flatMap(sheetId => Option(sheets.get(sheetId)).map(_.getCellValues))

}
