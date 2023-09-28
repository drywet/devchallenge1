package pkg

import pkg.CalcParser.{evaluate, Expr}

sealed trait CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double]
}

case class CellValueNumber(value: Double) extends CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = Some(value)
}

/** @param value Non-empty string */
case class CellValueString(value: String) extends CellValueParsed {
  require(value.nonEmpty, "Empty string values should be represented as CellValueEmpty")
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = None
}

case object CellValueEmpty extends CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = Some(0.0)
}

case class CellValueExpr(value: Expr) extends CellValueParsed {
  def number()(implicit cellEvaluator: CellEvaluator): Option[Double] = evaluate(value)
}

/** @param parsed None on error, Some otherwise */
case class CellValue(source: String, parsed: Option[CellValueParsed])

object CellValue {
  val Empty: CellValue = CellValue("", None)
}

case class Cell(name: String, var value: CellValue, var beingEvaluated: Boolean)
