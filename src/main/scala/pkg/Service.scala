package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import io.activej.http.HttpHeaderValue.ofContentType
import io.activej.http.HttpHeaders.CONTENT_TYPE
import io.activej.http.MediaTypes.JSON
import io.activej.http.{AsyncHttpClient, ContentType, HttpRequest}
import org.slf4j.{Logger, LoggerFactory}
import pkg.CalcParser.{Expr, parseExpression}
import pkg.Model.{BackwardPass, CellUpdate, DbItemParsed, DbKey, DbValue, ForwardPass, Pass, SubscribeToCellWebhookRequest, evaluatedResultFormat}
import pkg.StringUtils.{checkUrl, normalizeFormula, normalizeId}

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

class Service(db: Option[Db], httpClient: AsyncHttpClient) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()

  db.foreach { db =>
    // Restore the state from the database
    loadDbItems(db) match {
      case Success(sheetItems) =>
        val sheetItemsMap: Map[String, Seq[DbItemParsed]] = sheetItems.groupMap(_._1)(_._2)
        sheetItemsMap.foreach { case (sheetId, items) =>
          sheets.put(sheetId, new SheetImpl(sheetId, Some(db)))
          val itemsMap: Map[String, DbItemParsed] = items.groupMapReduce(_.cellId)(identity)((a, _) => a)
          val sorted: IndexedSeq[DbItemParsed]    = allItemsTopologicallySorted(itemsMap)
          sorted.reverseIterator.foreach { item =>
            if (putCell(sheetId, item.cellId, item.value).isEmpty) {
              logger.warn(s"Couldn't restore a value for cell ${item.cellId}")
            }
          }
        }
      case Failure(e) =>
        logger.warn("Couldn't load data from DB", e)
    }
  }

  private def loadDbItems(db: Db): Try[Seq[(String, DbItemParsed)]] = {
    Using(db.db.newIterator()) { iterator =>
      iterator.seekToFirst()
      val sheetItems = ArraySeq.newBuilder[(String, DbItemParsed)]
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
        sheetItems += (dbKey.sheetId -> dbItemParsed)
        iterator.next()
      }
      sheetItems.result()
    }
  }

  private def allItemsTopologicallySorted(
      itemsMap: Map[String, DbItemParsed]
  ): IndexedSeq[DbItemParsed] = {
    val sorted                                     = mutable.ArrayDeque[DbItemParsed]()
    val stack: mutable.Stack[(DbItemParsed, Pass)] = mutable.Stack.from(itemsMap.values.map(_ -> ForwardPass))

    while (stack.nonEmpty) {
      val (item, pass) = stack.pop()
      pass match {
        case ForwardPass =>
          if (!item.traversed) {
            item.traversed = true
            stack.push((item, BackwardPass))
            item.referencedVariables.foreach { referencedItemName =>
              val referencedItem = itemsMap(referencedItemName)
              if (!referencedItem.traversed) {
                stack.push((referencedItem, ForwardPass))
              }
            }
          }
        case BackwardPass =>
          sorted.prepend(item)
      }
    }
    sorted.foreach(_.traversed = false)
    sorted.toIndexedSeq
  }

  def putCell(sheetId: String, cellId: String, sourceValue: String): Option[Either[String, Double]] = {
    (normalizeId(sheetId), normalizeId(cellId)) match {
      case (Some(sheetId), Some(cellId)) =>
        val sheet = sheets.computeIfAbsent(sheetId, sheetId => new SheetImpl(sheetId, db))
        val result: Option[(Either[String, Double], Seq[Model.CellUpdate])] =
          sheet.putCellValue(id = cellId, sourceValue = sourceValue)
        result.map(_._2).foreach(callCellUpdateWebhooks)
        result.map(_._1)
      case _ =>
        None
    }
  }

  private def callCellUpdateWebhooks(updates: Seq[CellUpdate]): Unit = {
    // logger.info(s"updates: $updates")
    updates.foreach { update =>
      val jsonStr = writeToString(
        SubscribeToCellWebhookRequest(value = update.sourceValue, result = evaluatedResultFormat(update.result))
      )
      httpClient
        .request(
          HttpRequest
            .post(update.webhookUrl)
            .withHeader(CONTENT_TYPE, ofContentType(ContentType.of(JSON)))
            .withBody(jsonStr.getBytes(UTF_8))
        )
        .async()
    }
  }

  def getCell(sheetId: String, cellId: String): Option[(String, Either[String, Double])] =
    (normalizeId(sheetId), normalizeId(cellId)) match {
      case (Some(sheetId), Some(cellId)) =>
        Option(sheets.get(sheetId)).flatMap(_.getCellValue(cellId))
      case _ =>
        None
    }

  def subscribeToCell(sheetId: String, cellId: String, webhookUrl: String): Option[Unit] = {
    (normalizeId(sheetId), normalizeId(cellId), checkUrl(webhookUrl)) match {
      case (Some(sheetId), Some(cellId), true) =>
        Option(sheets.get(sheetId)) match {
          case Some(sheet) =>
            Some(sheet.addCellSubscription(cellId, webhookUrl))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def getSheet(sheetId: String): Option[Map[String, (String, Either[String, Double])]] =
    normalizeId(sheetId).flatMap(sheetId => Option(sheets.get(sheetId)).map(_.getCellValues))

}
