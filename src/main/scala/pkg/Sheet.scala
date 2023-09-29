package pkg

import org.parboiled2.ParseError
import pkg.StringUtils.normalizeFormula

import java.util.concurrent.locks.StampedLock
import scala.collection.immutable.ArraySeq
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

// TODO Increase stack size and add a tests for long deps chains updates at the bottom and at the top

// TODO Add HTTP API and choose the optimal number of worker threads

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

  /** Performance characteristics:
   * write complexity: tree traversals for re-evaluation are log(n) on average and n in the worst case
   * read complexity: 1
   * 
   * Algorithm description:
   * On adding a cell, the expression is evaluated shallowly since all cells store cached evaluated values
   * If a cell is updated (not created) then cells in the expression (bottom cells) are traversed in depth to make sure there are no circular dependencies.
   *   Update the existing cell's value. Store a list of bottom cells no longer referenced in the cell expression
   *   Circular dependencies are detected by comparing the cell currently traversed vs the cell updated
   *   Each cell should store a set of cells it directly depends on, for fast tree traversal. It's specified together with the parsed value
   *   Each cell also stores a set of cells that directly depend on it. It's updated at the end on success, after all checks
   * Otherwise, if the cell doesn't exist, create the cell
   * If the expression and bottom cells are ok or the value being stored is a number/string
   *   If the cell is updated (not created) then check cells that depend on this cell (top cells)
   *     Traverse top cells of the cell and sort them topologically in a list
   *     Evaluate all cells in that order:
   *       Set previousEvaluated=evaluated
   *       Set evaluated=evaluate()
   *     If any evaluation fails, restore previous evaluated values for the updated cells
   *     Set previousEvaluated=None and traversed=false for all evaluated cells
   * If evaluation or deps checks fail, the cell value is reverted or the cell is removed;
   * otherwise, on success, put the cell to the top cells sets of its bottom cells and remove the cell from the top cells sets of no longer referenced cells 
   *
   * @return evaluatedResult: None on error, Some otherwise */
  def putCellValue(name: String, sourceValue: String): Option[Either[String, Double]] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.writeLock()
    try
      putCellValueImpl(name = name, sourceValue = sourceValue)
    finally
      lock.unlockWrite(lockStamp)
  }

  /** @return evaluatedResult: None on error, Some otherwise */
  private def putCellValueImpl(name: String, sourceValue: String): Option[Either[String, Double]] = {
    val existingCell      = cells.get(name)
    val previousCellValue = existingCell.map(_.value)
    val result: Option[(Cell, Set[Cell])] =
      parseAndEvaluateSourceValue(name = name, sourceValue = sourceValue).flatMap {
        case (parsedValue, evaluatedResult) =>
          val (cell, noLongerReferencedCells) = createOrUpdateCell(
            existingCell = existingCell,
            name = name,
            sourceValue = sourceValue,
            parsedValue = parsedValue,
            evaluatedResult = evaluatedResult
          )
          if (!hasCircularDeps(cell)) {
            val topCells: ArraySeq[Cell] = allTopCellsTopologicallySorted(cell)
            var i                        = 0
            var evaluationFailed         = false
            while (i < topCells.size && !evaluationFailed) {
              topCells(i).value.parsed.evaluate()(cellEvaluator = this) match {
                case Some(evaluatedResult) =>
                  topCells(i).value.previousEvaluated = Some(topCells(i).value.evaluated)
                  topCells(i).value.evaluated = evaluatedResult
                case None =>
                  evaluationFailed = true
              }
              i += 1
            }
            if (evaluationFailed) {
              (0 until (i - 1)).foreach(j => topCells(j).value.evaluated = topCells(j).value.previousEvaluated.get)
            }
            (0 until i).foreach { j =>
              topCells(j).value.previousEvaluated = None
              topCells(j).value.traversed = false
            }
            if (!evaluationFailed) Some(cell -> noLongerReferencedCells) else None
          } else None
      }
    result match {
      case Some((cell, noLongerReferencedCells)) =>
        cell.value.bottomCells.foreach(_.value.topCells += cell)
        noLongerReferencedCells.foreach(_.value.topCells -= cell)
        Some(cell.value.evaluated)
      case None =>
        existingCell match {
          case Some(existingCell) => existingCell.value = previousCellValue.get
          case None               => cells.remove(name)
        }
        None
    }
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

  /** @return (cell, no longer referenced cells) */
  private def createOrUpdateCell(
      existingCell: Option[Cell],
      name: String,
      sourceValue: String,
      parsedValue: CellValueParsed,
      evaluatedResult: Either[String, Double]
  ): (Cell, Set[Cell]) = {
    existingCell match {
      case Some(existingCell) =>
        val bottomCells             = referencedCells(parsedValue)
        val noLongerReferencedCells = existingCell.value.bottomCells -- bottomCells
        existingCell.value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = existingCell.value.topCells,
          bottomCells = bottomCells,
          evaluated = evaluatedResult,
          previousEvaluated = None,
          traversed = false
        )
        existingCell -> noLongerReferencedCells
      case None =>
        val value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = mutable.Set(),
          bottomCells = referencedCells(parsedValue),
          evaluated = evaluatedResult,
          previousEvaluated = None,
          traversed = false
        )
        val cell = new Cell(name, value)
        cells.put(name, cell)
        cell -> Set.empty
    }
  }

  private def referencedCells(parsedValue: CellValueParsed): Set[Cell] =
    parsedValue match {
      case CellValueExpr(expr) => CalcParser.referencedVariables(expr).map(cells(_))
      case _                   => Set.empty
    }

  private def hasCircularDeps(cell: Cell): Boolean = {
    def loop(cell2: Cell): Boolean =
      (cell == cell2) || cell2.value.bottomCells.exists(loop)

    cell.value.bottomCells.exists(loop)
  }

  // TODO Test
  /** @return All top cells sorted such that no cell mentions any of subsequent cells in its topCells set */
  def allTopCellsTopologicallySorted(cell: Cell): ArraySeq[Cell] = {
    val sorted: mutable.Builder[Cell, ArraySeq[Cell]] = ArraySeq.newBuilder

    def loop(cell: Cell): Unit = {
      cell.value.traversed = true
      cell.value.topCells.foreach { cell2 =>
        if (!cell2.value.traversed)
          loop(cell2)
      }
      sorted += cell
    }

    cell.value.topCells.foreach { cell =>
      if (!cell.value.traversed)
        loop(cell)
    }

    sorted.result()
  }

  /** @return None if the cell doesn't exist, Some otherwise */
  override def getEvaluatedCellValue(name: String): Option[Either[String, Double]] =
    cells.get(name).map(_.value.evaluated)

}
