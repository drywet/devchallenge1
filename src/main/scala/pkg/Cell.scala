package pkg

import pkg.CalcParser.Expr

import scala.collection.mutable

sealed trait CellValueParsed {
  def evaluate()(implicit cellEvaluator: CellEvaluator): Option[Either[String, Double]]
}

case class CellValueNumber(value: Double) extends CellValueParsed {
  def evaluate()(implicit cellEvaluator: CellEvaluator): Option[Right[String, Double]] = Some(Right(value))
}

/** @param value Non-empty string */
case class CellValueString(value: String) extends CellValueParsed {
  require(value.nonEmpty, "Empty string values are not supported")
  def evaluate()(implicit cellEvaluator: CellEvaluator): Option[Left[String, Double]] = Some(Left(value))
}

case class CellValueExpr(value: Expr) extends CellValueParsed {
  def evaluate()(implicit cellEvaluator: CellEvaluator): Option[Either[String, Double]] = CalcParser.evaluate(value)
}

/** @param parsed parsed value: number/string/expression 
 * @param topCells cells that mention this cell in their expressions
 * @param bottomCells cells this cell mentions in the expression
 * @param evaluated cached evaluated value of [[parsed]] param             
 * @param tempEvaluated Used during bottom-up traversal during cell update/creation, and is cleared afterwards */
case class CellValue(
    parsed: CellValueParsed,
    topCells: mutable.Set[Cell],
    bottomCells: Set[Cell],
    evaluated: Either[String, Double],
    var tempEvaluated: Option[Either[String, Double]]
)

/** In a Sheet, every cell has only one unique instance, so it's possible to compare cells by reference for better performance, 
 * keeping the default equals/hashCode implementation, hence this class isn't a case class. 
 * @param source Original string value to be parsed as a number/string/expression
 * @param value None value is possible only for cells that are being created.
 *               On success, the value is set to Some, and on failure the cell is removed */
class Cell(
    val name: String,
    val source: String,
    var value: Option[CellValue],
    var beingEvaluated: Boolean // TODO remove this field and replace with reference check against this cell
)

object Cell {
  def empty(name: String, source: String): Cell = new Cell(
    name = name,
    source = source,
    value = None,
    beingEvaluated = false
  )
}
