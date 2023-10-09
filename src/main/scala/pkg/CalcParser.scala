package pkg

import org.parboiled2.CharPredicate.Digit
import org.parboiled2._
import org.slf4j.{Logger, LoggerFactory}
import pkg.Model.{BackwardPass, ForwardPass, Pass}

import scala.collection.mutable
import scala.util.{Failure, Success}

/** Based on <a href="https://github.com/sirthias/parboiled2/blob/master/examples/src/main/scala/org/parboiled2/examples/Calculator2.scala">Calculator2.scala</a>
 * Extended with support for float numbers and variables */
object CalcParser {

  private val debug: Boolean = false

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  private val Dot: CharPredicate       = CharPredicate('.')
  private val E: CharPredicate         = CharPredicate('e', 'E')
  private val PlusMinus: CharPredicate = CharPredicate('+', '-')
  // All except digits and arithmetic ops
  // https://jrgraphix.net/r/Unicode
  private val AlphaExtended: CharPredicate = CharPredicate(
    ('\u0000' to '\u0027') ++
      ('\u002c' to '\u002c') ++
      ('\u002e' to '\u002e') ++
      ('\u003a' to '\uffef')
  )
  // All except arithmetic ops
  private val AlphaExtendedNumeric: CharPredicate = CharPredicate(
    ('\u0000' to '\u0027') ++
      ('\u002c' to '\u002c') ++
      ('\u002e' to '\u002e') ++
      ('\u0030' to '\uffef')
  )

  def parseExpression(cellId: String, formula: String): Option[Expr] = {
    val parser = new CalcParser(formula)
    parser.InputLine.run() match {
      case Success(expr) =>
        Some(expr)
      case Failure(e: ParseError) =>
        if (debug) logger.warn(s"Cell $cellId expression is not valid: ${parser.formatError(e)}")
        None
      case Failure(e) =>
        if (debug) logger.warn(s"Cell $cellId unexpected error during parsing run: $e")
        None
    }
  }

  def evaluate(expr: Expr)(implicit cellEvaluator: CellEvaluator): Option[Either[String, Double]] = {
    expr match {
      case NumberValue(v)      => v.toDoubleOption.map(Right(_))
      case VariableValue(name) => cellEvaluator.getEvaluatedCellValue(name)
      case _ =>
        val stack      = mutable.Stack[(Expr, Pass)](expr -> ForwardPass)
        val valueStack = mutable.Stack[Option[Double]]()
        while (stack.nonEmpty) {
          stack.pop() match {
            case (NumberValue(v), ForwardPass) => valueStack.push(v.toDoubleOption)
            case (VariableValue(name), ForwardPass) =>
              valueStack.push(cellEvaluator.getEvaluatedCellValue(name).flatMap(valueToNumber))
            case (op: BinOp, ForwardPass) =>
              stack.push(op     -> BackwardPass)
              stack.push(op.lhs -> ForwardPass)
              stack.push(op.rhs -> ForwardPass)
            case (op: BinOp, BackwardPass) =>
              val res = for {
                a <- valueStack.pop()
                b <- valueStack.pop()
              } yield evaluateOp(op, a, b)
              valueStack.push(res)
          }
        }
        valueStack.top.map(Right(_))
    }
  }

  private def valueToNumber(value: Either[String, Double]): Option[Double] = value match {
    case Right(value)                 => Some(value)
    case Left(value) if value.isEmpty => Some(0)
    case _                            => None
  }

  private def evaluateOp(op: BinOp, a: Double, b: Double) = op match {
    case _: Addition       => a + b
    case _: Subtraction    => a - b
    case _: Multiplication => a * b
    case _: Division       => a / b
  }

  def referencedVariables(expr: Expr): Set[String] = {
    val variables: mutable.Builder[String, Set[String]] = Set.newBuilder[String]
    val stack: mutable.Stack[Expr]                      = mutable.Stack(expr)

    while (stack.nonEmpty) {
      val expr = stack.removeLast()
      expr match {
        case NumberValue(_)       =>
        case VariableValue(name)  => variables += name
        case Addition(a, b)       => stack.append(a); stack.append(b)
        case Subtraction(a, b)    => stack.append(a); stack.append(b)
        case Multiplication(a, b) => stack.append(a); stack.append(b)
        case Division(a, b)       => stack.append(a); stack.append(b)
      }
    }

    variables.result()
  }

  // Abstract syntax tree model
  sealed trait Expr

  sealed trait BinOp extends Expr {
    def lhs: Expr
    def rhs: Expr
  }

  case class NumberValue(value: String)  extends Expr
  case class VariableValue(name: String) extends Expr

  case class Addition(lhs: Expr, rhs: Expr)       extends BinOp
  case class Subtraction(lhs: Expr, rhs: Expr)    extends BinOp
  case class Multiplication(lhs: Expr, rhs: Expr) extends BinOp
  case class Division(lhs: Expr, rhs: Expr)       extends BinOp

}

/** This parser reads calculator expressions and builds an AST
 * for them, to be evaluated in a separate phase, after parsing is completed.
 */
class CalcParser(val input: ParserInput) extends Parser {

  import CalcParser._

  def InputLine: Rule1[Expr] = rule(Expression ~ EOI)

  private def Expression: Rule1[Expr] =
    rule {
      Term ~ zeroOrMore(
        '+' ~ Term ~> Addition.apply _
          | '-' ~ Term ~> Subtraction.apply _
      )
    }

  private def Term: Rule1[Expr] =
    rule {
      Factor ~ zeroOrMore(
        '*' ~ Factor ~> Multiplication.apply _
          | '/' ~ Factor ~> Division.apply _
      )
    }

  private def Factor: Rule1[Expr] = rule(Number | Variable | Parens)

  private def Parens: Rule1[Expr] = rule('(' ~ Expression ~ ')')

  private def Number: Rule1[NumberValue] = rule(capture(Float) ~> NumberValue.apply _)

  /** No support for Infinity / -Infinity / NaN */
  private def Float: Rule0 = rule(
    optional(PlusMinus) ~ FloatDigits ~ optional(FloatExponent)
  )

  private def FloatDigits: Rule0 = rule(
    (oneOrMore(Digit) ~ optional(Dot) ~ zeroOrMore(Digit)) |
      (Dot ~ oneOrMore(Digit))
  )

  private def FloatExponent: Rule0 = rule(
    E ~ optional(PlusMinus) ~ oneOrMore(Digit)
  )

  private def Variable: Rule1[VariableValue] =
    // No support for variables starting with a digit, because `1e1` should be a number, not a variable
    rule(capture(AlphaExtended ~ zeroOrMore(AlphaExtendedNumeric)) ~> VariableValue.apply _)

}
