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
  def getEvaluatedCellValue(name: String): Option[Either[String, Double]]
}

// TODO Don't store values that break other values. This reverse dependency tracking can be combined with the write cache.
// -- On adding a cell, the expression is evaluated shallowly since all cells store cached evaluated values;
// -- If a cell is updated (not created) then cells in the expression (bottom cells) are traversed in depth to make sure there are no circular dependencies.
//   -- Update the existing cell's value
//   -- Circular dependencies are detected by comparing the cell currently traversed vs the cell updated
//   -- Each cell should store a set of cells it directly depends on, for fast tree traversal. It's specified together with the parsed value
//   -- Each cell also stores a set of cells that directly depend on it. It's updated at the end on success, after all checks
// -- Otherwise, if the cell doesn't exist, create the cell
// If the expression and bottom cells are ok or the value being stored is a number/string
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

  /** @return Option[(sourceValue, evaluatedResult)] - Some if the cell exists, None otherwise */
  def getCellValue(name: String): Option[(String, Either[String, Double])] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.readLock()
    val result: Option[(String, Either[String, Double])] =
      cells.get(name).map(cell => cell.value.source -> cell.value.evaluated)
    lock.unlockRead(lockStamp)
    result
  }

  /** @return evaluatedResult: None on error, Some otherwise */
  def putCellValue(name: String, sourceValue: String): Option[Either[String, Double]] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.writeLock()
    try
      putCellValue2(name, sourceValue)
    finally
      lock.unlockWrite(lockStamp)
  }

  /** @return evaluatedResult: None on error, Some otherwise */
  private def putCellValue2(name: String, sourceValue: String): Option[Either[String, Double]] = {
    val cellOpt           = cells.get(name)
    val previousCellValue = cellOpt.map(_.value)
    parseAndEvaluateSourceValue(name, sourceValue).flatMap { case (parsedValue, evaluatedResult) =>
      val cell = createOrUpdateCell(cellOpt, name, sourceValue, parsedValue, evaluatedResult)
      if (!hasCircularDeps(cell)) {

        Some(cell)
      } else None
    }
    // if (evaluatedResult.isDefined) {
    //   require(parsedValue.isDefined)
    //   cell.value = Some(CellValue(parsedValue.get))
    // } else {
    //   previousCellValue match {
    //     case Some(previousValue) => cell.value = previousValue
    //     case None                => cells.remove(cell.name)
    //   }
    // }
    // evaluatedResult
    ()
  }

  /** @return Option[(parsedValue, evaluatedResult)] */
  private def parseAndEvaluateSourceValue(
      name: String,
      sourceValue: String
  ): Option[(CellValueParsed, Either[String, Double])] = {
    if (sourceValue.startsWith("=")) {
      val formula = normalizeFormula(sourceValue.drop(1))
      val parser  = new CalcParser(formula)
      parser.InputLine.run() match {
        case Success(expr) =>
          val evaluatedResult = CalcParser.evaluate(expr)(cellEvaluator = this)
          evaluatedResult.map(evaluatedResult => CellValueExpr(expr) -> evaluatedResult)
        case Failure(e: ParseError) =>
          if (debug) println(s"Cell $name Expression is not valid: ${parser.formatError(e)}")
          None
        case Failure(e) =>
          if (debug) println(s"Cell $name Unexpected error during parsing run: $e")
          None
      }
    } else if (sourceValue.toDoubleOption.isDefined) {
      val number = sourceValue.toDouble
      Some(CellValueNumber(number) -> Right(number))
    } else if (sourceValue.nonEmpty) {
      Some(CellValueString(sourceValue) -> Left(sourceValue))
    } else {
      None
    }
  }

  private def createOrUpdateCell(
                                  existingCell: Option[Cell],
                                  name: String,
                                  sourceValue: String,
                                  parsedValue: CellValueParsed,
                                  evaluatedResult: Either[String, Double]
                                ) = {
    existingCell match {
      case Some(existingCell) =>
        existingCell.value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = existingCell.value.topCells,
          bottomCells = referencedCells(parsedValue),
          evaluated = evaluatedResult,
          tempEvaluated = None
        )
        existingCell
      case None =>
        val value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = mutable.Set(),
          bottomCells = referencedCells(parsedValue),
          evaluated = evaluatedResult,
          tempEvaluated = None
        )
        val cell = new Cell(name, value)
        cells.put(name, cell)
        cell
    }
  }

  private def hasCircularDeps(cell: Cell): Boolean = {
    def loop(cell2: Cell): Boolean = {
      (cell == cell2) || cell2.value.bottomCells.exists(loop)
    }

    loop(cell)
  }

  private def referencedCells(parsedValue: CellValueParsed): Set[Cell] =
    parsedValue match {
      case CellValueExpr(expr) => CalcParser.referencedVariables(expr).map(cells.get)
      case _                   => Set.empty
    }

  /** @return None if the cell doesn't exist, Some otherwise */
  override def getEvaluatedCellValue(name: String): Option[Either[String, Double]] =
    cells.get(name).map(_.value.evaluated)

}
