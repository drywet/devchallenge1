package pkg

import org.parboiled2.ParseError
import pkg.CalcParser.evaluate
import pkg.StringUtils.normalizeFormula

import java.util.concurrent.locks.StampedLock
import scala.collection.mutable
import scala.util.{Failure, Success}

trait Sheet {
  def getCellValue(name: String): Option[(String, Either[String, Double])]
  def putCellValue(name: String, sourceValue: String): Option[Either[String, Double]]
}

trait CellEvaluator {
  /** @return None if the cell doesn't exist, Some otherwise */
  def getAndEvaluateCell(name: String): Option[Either[String, Double]]
}

// TODO Don't store values that break other values. This reverse dependency tracking can be combined with the write cache.
// On adding a cell, the expression is evaluated shallowly since all cells store cached evaluated values;
// If a cell is updated (not created) then cells in the expression (bottom cells) are traversed in depth to make sure there are no circular dependencies.
//   Circular dependencies are detected by comparing the cell currently traversed vs the cell updated
//   Each cell should store a set of cells it directly depends on, for fast tree traversal. It's specified together with the parsed value
//   Each cell also stores a set of cells that directly depend on it. It's updated at the end on success after all checks
// If the expression and bottom cells are ok or the value being stored is a number/string
//   Create the cell or update the existing cell's value
//   if the cell is updated (not created) then check cells that depend on this cell (top cells)
//     Depth-traversal: temporary evaluated value is stored to another tempEvaluated field; All traversed top cells are added to a list.
//       Note: during cell evaluation, tempEvaluated should be used if it is present, and `evaluated` otherwise
//     after evaluation:
//      if ok, for all traversed top cells, copy tempEvaluated to `evaluated`;
//      in any case - set all traversed tempEvaluated back to None
// If evaluation or deps checks fail, the cell value is reverted or the cell is removed, and 422 is returned.
// otherwise, on success, put the cell to the top cells sets of its bottom cells

// TODO Increase stack size and add a tests for long deps chains updates at the bottom and at the top

// TODO tree with propagation on writes:
//   write: 1 + tree propagation (log n in the average case, n in the worst case)
//   read: 1
class SheetImpl extends Sheet with CellEvaluator {

  private val debug: Boolean = false

  private val cells: mutable.HashMap[String, Cell] = new mutable.HashMap()
  private val lock: StampedLock                    = new StampedLock()

  /** @return Option[sourceValue, evaluatedResult] - Some if the cell is defined, None otherwise.
   *         sourceValue: String, evaluatedResult: None or error, Some otherwise */
  def getCellValue(name: String): Option[(String, Either[String, Double])] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.readLock()
    val result: Option[(String, Option[Either[String, Double]])] =
      try {
        cells.get(name).map { cell =>
          val evaluatedResult: Option[Either[String, Double]] = cell.value.flatMap(_.parsed match {
            case x: CellValueNumber => Some(Right(x.value))
            case x: CellValueString => Some(Left(x.value))
            case x: CellValueExpr   => x.evaluate()(cellEvaluator = this)
          })
          cell.source -> evaluatedResult
        }
      } finally
        lock.unlockRead(lockStamp)
    result match {
      case Some((_, None)) =>
        throw new IllegalStateException(s"Cell '$name' evaluation failed")
      case Some((source, Some(evaluatedResult))) =>
        Some(source, evaluatedResult)
      case None =>
        None
    }
  }

  /** @return evaluatedResult: None or error, Some otherwise */
  def putCellValue(name: String, sourceValue: String): Option[Either[String, Double]] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.writeLock()
    try {
      val previousCellValue = cells.get(name).map(_.value)
      val cell              = getCellOrCreate(name, sourceValue)
      val result: (Option[CellValueParsed], Option[Either[String, Double]]) =
        if (sourceValue.startsWith("=")) {
          val formula = normalizeFormula(sourceValue.drop(1))
          val parser  = new CalcParser(formula)
          parser.InputLine.run() match {
            case Success(expr) =>
              val evaluatedResult = evaluate(expr)(cellEvaluator = this)
              Some(CellValueExpr(expr)) -> evaluatedResult
            case Failure(e: ParseError) =>
              if (debug) println("Expression is not valid: " + parser.formatError(e))
              None -> None
            case Failure(e) =>
              if (debug) println("Unexpected error during parsing run: " + e)
              None -> None
          }
        } else if (sourceValue.toDoubleOption.isDefined) {
          val number = sourceValue.toDouble
          Some(CellValueNumber(number)) -> Some(Right(number))
        } else if (sourceValue.nonEmpty) {
          (Some(CellValueString(sourceValue)), Some(Left(sourceValue)))
        } else {
          None -> None
        }
      val (parsedValue, evaluatedResult) = result
      if (evaluatedResult.isDefined) {
        require(parsedValue.isDefined)
        cell.value = Some(CellValue(parsedValue.get))
      } else {
        previousCellValue match {
          case Some(previousValue) => cell.value = previousValue
          case None                => cells.remove(cell.name)
        }
      }
      evaluatedResult
    } finally
      lock.unlockWrite(lockStamp)
  }

  /** @return None if the cell doesn't exist, Some otherwise */
  override def getAndEvaluateCell(name: String): Option[Either[String, Double]] =
    cells.get(name).map(_.value.evaluated)

}
