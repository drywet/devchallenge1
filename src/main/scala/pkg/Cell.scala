package pkg

import pkg.CalcParser.{Expr, evaluate}

sealed trait CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double]
}

case class CellValueNumber(value: Double) extends CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = Some(value)
}

/** @param value Non-empty string */
case class CellValueString(value: String) extends CellValueParsed {
  require(value.nonEmpty, "Empty string values are not supported")
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = None
}

case class CellValueExpr(value: Expr) extends CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = evaluate(value)
}

/** In a Sheet, every cell has only one unique instance, so it's possible to compare cells by reference, 
 * keeping the default equals/hashCode implementation, hence this class isn't a case class.
 * @param source Original string value to be parsed as a number/string/expression
 * @param parsed None value is possible only for cells that are being created.
 *               On success, the value is set to Some, and on failure the cell is removed */
class CellValue(val source: String, val parsed: Option[CellValueParsed])

object CellValue {
  val Empty: CellValue = new CellValue("", None)
}

case class Cell(name: String, var value: CellValue, var beingEvaluated: Boolean)
