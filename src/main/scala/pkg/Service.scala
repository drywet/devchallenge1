package pkg

import pkg.DoubleUtils.cellDoubleFormat

import java.util.concurrent.ConcurrentHashMap

class Service {

  private val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()

  def putCell(sheetId: String, cellId: String, sourceValue: String): Option[Either[String, Double]] = {
    // TODO sheetId, cellId - no whitespace supported; don't trim
    // TODO expr normalizer - removing whitespace may make incorrect formulas correct!
    val sheet = sheets.computeIfAbsent(sheetId, sheetId => new SheetImpl(sheetId))
    sheet.putCellValue(id = cellId, sourceValue = sourceValue)
  }

}
