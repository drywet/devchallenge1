package pkg

import org.parboiled2.ParseError
import pkg.CalcParser.evaluate
import pkg.DoubleUtils.cellDoubleFormat
import pkg.StringUtils.normalize

import java.util.concurrent.locks.StampedLock
import scala.collection.mutable
import scala.util.{Failure, Success}

trait Sheet {
  def getCellValue(name: String): Option[(String, Option[String])]
  def putCellValue(name: String, sourceValue: String): Option[String]
}

trait CellEvaluator {
  def getAndEvaluateCellAsNumber(name: String): Option[Double]
}

// TODO tree with propagation on writes:
//   write: 1 + tree propagation (log n in the average case, n in the worst case)
//   read: 1
class SheetImpl extends Sheet with CellEvaluator {

  private val cells: mutable.HashMap[String, Cell] = new mutable.HashMap()
  private val lock: StampedLock                    = new StampedLock()

  /** @return Option[sourceValue, evaluatedResult] - Some if the cell is defined, None otherwise.
   *         sourceValue: String, evaluatedResult: None or error, Some otherwise. Empty cell value => Some(0.0) */
  def getCellValue(name: String): Option[(String, Option[String])] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp = lock.readLock()
    val result: Option[(String, Option[String])] = cells.get(name).map { cell =>
      if (cell.beingEvaluated) {
        throw new IllegalStateException(
          s"Cell '$name' beingEvaluated=true on cell get is unexpected here " +
            s"because this thread acquired a read access lock to all cells"
        )
      } else {
        val evaluatedResult: Option[String] = cell.value.parsed match {
          case Some(parsed) =>
            parsed match {
              case x: CellValueNumber => Some(cellDoubleFormat(x.value))
              case x: CellValueString => Some(x.value)
              case x: CellValueExpr   => x.number()(cellEvaluator = this).map(cellDoubleFormat)
            }
          case None => None
        }
        cell.value.source -> evaluatedResult
      }
    }
    lock.unlockRead(lockStamp)
    result
  }

  /** @return evaluatedResult: None or error, Some otherwise. Empty cell value => Some(0.0) */
  def putCellValue(name: String, sourceValue: String): Option[String] = {
    require(name.trim.nonEmpty, "Cell name should be non-empty")
    val lockStamp         = lock.writeLock()
    val previousCellValue = cells.get(name).map(_.value)
    val cell              = getCellOrCreate(name)
    val evaluatedResult: Option[String] =
      if (cell.beingEvaluated) {
        throw new IllegalStateException(
          s"Cell '$name' beingEvaluated=true on cell put is unexpected here " +
            s"because this thread acquired exclusive access to all cells"
        )
      } else {
        cell.beingEvaluated = true
        val (parsedValue: Option[CellValueParsed], evaluatedResult: Option[String]) =
          if (sourceValue.startsWith("=")) {
            val formula = normalize(sourceValue.drop(1))
            val parser  = new CalcParser(formula)
            parser.InputLine.run() match {
              case Success(expr) =>
                val evaluatedResult: Option[Double] = evaluate(expr)(cellEvaluator = this)
                Some(CellValueExpr(expr)) -> evaluatedResult.map(cellDoubleFormat)
              case Failure(e: ParseError) =>
                println("Expression is not valid: " + parser.formatError(e))
                None -> None
              case Failure(e) =>
                println("Unexpected error during parsing run: " + e)
                None -> None
            }
          } else if (sourceValue.toDoubleOption.isDefined) {
            val number = sourceValue.toDouble
            Some(CellValueNumber(number)) -> Some(cellDoubleFormat(number))
          } else if (sourceValue.nonEmpty) {
            Some(CellValueString(sourceValue)) -> Some(sourceValue)
          } else {
            None -> None
          }
        cell.value = CellValue(sourceValue, parsedValue)
        cell.beingEvaluated = false
        evaluatedResult
      }
    if (evaluatedResult.isEmpty) {
      previousCellValue match {
        case Some(value) => cell.value = value
        case None        => cells.remove(cell.name)
      }
    }
    lock.unlockWrite(lockStamp)
    evaluatedResult
  }

  private def getCellOrCreate(name: String): Cell =
    cells.getOrElseUpdate(name, Cell(name, CellValue.Empty, beingEvaluated = false))

  /** @return None on error, Some otherwise. Empty cell value => Some(0.0) */
  override def getAndEvaluateCellAsNumber(name: String): Option[Double] =
    cells.get(name) match {
      case Some(cell) => evaluateCellAsNumber(cell)
      case None       => Some(0.0)
    }

  /** @return None on error, Some otherwise. Empty cell value => Some(0.0) */
  private def evaluateCellAsNumber(cell: Cell): Option[Double] = {
    if (cell.beingEvaluated) {
      None
    } else {
      cell.beingEvaluated = true
      val result = cell.value.parsed.flatMap(_.number()(cellEvaluator = this))
      cell.beingEvaluated = false
      result
    }
  }

}
