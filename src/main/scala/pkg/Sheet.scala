package pkg

import org.parboiled2.ParseError
import pkg.StringUtils.normalizeFormula

import java.util.concurrent.locks.StampedLock
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.{Failure, Success}

trait Sheet {

  /** @return Option[(sourceValue, evaluatedResult)] - Some if the cell exists, None otherwise */
  def getCellValue(id: String): Option[(String, Either[String, Double])]

  /** @return Map[cellId, (sourceValue, evaluatedResult)] */
  def getCellValues: Map[String, (String, Either[String, Double])]

  /** @return evaluatedResult: Some on success, None otherwise */
  def putCellValue(id: String, sourceValue: String): Option[Either[String, Double]]

}

trait CellEvaluator {

  /** @return Some if the cell exists, None otherwise */
  def getEvaluatedCellValue(id: String): Option[Either[String, Double]]

}

class SheetImpl(val sheetId: String) extends Sheet with CellEvaluator {

  private val debug: Boolean = false

  private val cells: mutable.HashMap[String, Cell] = new mutable.HashMap()
  private val lock: StampedLock                    = new StampedLock()

  /** @return Option[(sourceValue, evaluatedResult)] - Some if the cell exists, None otherwise */
  def getCellValue(id: String): Option[(String, Either[String, Double])] = {
    require(id.nonEmpty, "Cell id should be non-empty")
    val lockStamp = lock.readLock()
    val result: Option[(String, Either[String, Double])] =
      cells.get(id).map(cell => cell.value.source -> cell.value.evaluated)
    lock.unlockRead(lockStamp)
    result
  }

  /** @return Map[cellId, (sourceValue, evaluatedResult)] */
  def getCellValues: Map[String, (String, Either[String, Double])] = {
    val lockStamp = lock.readLock()
    val result    = cells.view.mapValues(cell => cell.value.source -> cell.value.evaluated).toMap
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
   *     Set previousEvaluated=None for all evaluated cells
   * If evaluation or deps checks fail, the cell value is reverted or the cell is removed;
   * otherwise, on success, put the cell to the top cells sets of its bottom cells and remove the cell from the top cells sets of no longer referenced cells 
   *
   * @return evaluatedResult: Some on success, None otherwise */
  def putCellValue(id: String, sourceValue: String): Option[Either[String, Double]] = {
    require(id.nonEmpty, "Cell id should be non-empty")
    val lockStamp = lock.writeLock()
    try
      putCellValueImpl(id = id, sourceValue = sourceValue)
    finally
      lock.unlockWrite(lockStamp)
  }

  /** @return evaluatedResult: Some on success, None otherwise */
  private def putCellValueImpl(id: String, sourceValue: String): Option[Either[String, Double]] = {
    val existingCell      = cells.get(id)
    val previousCellValue = existingCell.map(_.value)
    val result: Option[(Cell, Set[Cell], Set[Cell])] =
      // Optimisation: early check
      if (previousCellValue.forall(_.source != sourceValue)) {
        // Previous source value changed or doesn't exist
        parseAndEvaluateSourceValue(id = id, sourceValue = sourceValue).flatMap { case (parsedValue, evaluatedResult) =>
          val cell = createOrUpdateCell(
            existingCell = existingCell,
            id = id,
            sourceValue = sourceValue,
            parsedValue = parsedValue,
            evaluatedResult = evaluatedResult
          )

          // Optimisation: early checks
          previousCellValue match {
            case Some(previousCellValue) =>
              val newlyReferencedCells = cell.value.bottomCells -- previousCellValue.bottomCells
              if (!hasCircularDeps(cell, newlyReferencedCells)) {
                val evaluatedResultChanged = previousCellValue.evaluated != evaluatedResult
                if (!evaluatedResultChanged || reevaluateTopCells(cell)) {
                  val noLongerReferencedCells = previousCellValue.bottomCells -- cell.value.bottomCells
                  Some((cell, noLongerReferencedCells, newlyReferencedCells))
                } else None
              } else None
            case None =>
              Some((cell, Set.empty[Cell], cell.value.bottomCells))
          }
        }
      } else {
        // Previous source value exists, but didn't change
        Some((existingCell.get, Set.empty, Set.empty))
      }
    result match {
      case Some((cell, noLongerReferencedCells, newlyReferencedCells)) =>
        noLongerReferencedCells.foreach(_.value.topCells -= cell)
        newlyReferencedCells.foreach(_.value.topCells += cell)
        Some(cell.value.evaluated)
      case None =>
        existingCell match {
          case Some(existingCell) => existingCell.value = previousCellValue.get
          case None               => cells.remove(id)
        }
        None
    }
  }

  /** @return Option[(parsedValue, evaluatedResult)] */
  private def parseAndEvaluateSourceValue(
      id: String,
      sourceValue: String
  ): Option[(CellValueParsed, Either[String, Double])] = {
    if (sourceValue.startsWith("=")) {
      normalizeFormula(sourceValue.drop(1)).flatMap { formula =>
        val parser = new CalcParser(formula)
        parser.InputLine.run() match {
          case Success(expr) =>
            val evaluatedResult = CalcParser.evaluate(expr)(cellEvaluator = this)
            evaluatedResult.map(evaluatedResult => CellValueExpr(expr) -> evaluatedResult)
          case Failure(e: ParseError) =>
            if (debug) println(s"Cell $id Expression is not valid: ${parser.formatError(e)}")
            None
          case Failure(e) =>
            if (debug) println(s"Cell $id Unexpected error during parsing run: $e")
            None
        }
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
      id: String,
      sourceValue: String,
      parsedValue: CellValueParsed,
      evaluatedResult: Either[String, Double]
  ): Cell = {
    existingCell match {
      case Some(existingCell) =>
        val bottomCells = referencedCells(parsedValue)
        existingCell.value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = existingCell.value.topCells,
          bottomCells = bottomCells,
          evaluated = evaluatedResult,
          previousEvaluated = None,
          traversed = false
        )
        existingCell
      case None =>
        val bottomCells = referencedCells(parsedValue)
        val value = CellValue(
          source = sourceValue,
          parsed = parsedValue,
          topCells = mutable.Set(),
          bottomCells = bottomCells,
          evaluated = evaluatedResult,
          previousEvaluated = None,
          traversed = false
        )
        val cell = new Cell(id, value)
        cells.put(id, cell)
        cell
    }
  }

  private def referencedCells(parsedValue: CellValueParsed): Set[Cell] =
    parsedValue match {
      case CellValueExpr(expr) => CalcParser.referencedVariables(expr).map(cells(_))
      case _                   => Set.empty
    }

  private def hasCircularDeps(cell: Cell, newlyReferencedCells: Set[Cell]): Boolean = {
    def loop(cell2: Cell): Boolean =
      (cell == cell2) || cell2.value.bottomCells.exists(loop)

    newlyReferencedCells.exists(loop)
  }

  /** @return true on success, false otherwise */
  private def reevaluateTopCells(cell: Cell): Boolean = {
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
    (0 until i).foreach(j => topCells(j).value.previousEvaluated = None)
    !evaluationFailed
  }

  /** @return All top cells sorted such that no cell mentions in its topCells set any of the subsequent cells */
  private[pkg] def allTopCellsTopologicallySorted(cell: Cell): ArraySeq[Cell] = {
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

    // In-place reverse
    val reversed: ArraySeq[Cell] = sorted.result()
    val array: Array[Cell]       = reversed.unsafeArray.asInstanceOf[Array[Cell]]
    (0 until (reversed.length / 2)).foreach { i =>
      val a = array(i)
      array(i) = array(reversed.length - 1 - i)
      array(reversed.length - 1 - i) = a
    }

    reversed.foreach(_.value.traversed = false)

    reversed
  }

  /** @return Some if the cell exists, None otherwise */
  override def getEvaluatedCellValue(id: String): Option[Either[String, Double]] =
    cells.get(id).map(_.value.evaluated)

  /** For testing */
  private[pkg] def getCell(id: String): Option[Cell] = cells.get(id)

}
