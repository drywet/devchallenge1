package pkg

import pkg.StringUtils.normalizeId

import java.util.concurrent.ConcurrentHashMap

class Service {

  private val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()

  def putCell(sheetId: String, cellId: String, sourceValue: String): Option[Either[String, Double]] = {
    (normalizeId(sheetId), normalizeId(cellId)) match {
      case (Some(sheetId), Some(cellId)) =>
        val sheet = sheets.computeIfAbsent(sheetId, sheetId => new SheetImpl(sheetId))
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
