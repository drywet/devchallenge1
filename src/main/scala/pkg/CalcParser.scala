package pkg

import org.parboiled2.CharPredicate.Digit
import org.parboiled2._

import scala.collection.mutable

/** Based on <a href="https://github.com/sirthias/parboiled2/blob/master/examples/src/main/scala/org/parboiled2/examples/Calculator2.scala">Calculator2.scala</a>
 * Extended with support for float numbers and variables */
object CalcParser {

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

  /** Evaluate a parsed expression in iterative fashion. A recursive implementation is more concise than the iterative one, 
   * but is less performant and requires large thread stack size for calculating long expressions.
   * @return None on failure, Some otherwise */
  def evaluate(expr: Expr)(implicit cellEvaluator: CellEvaluator): Option[Either[String, Double]] = {
    val buffer: mutable.ArrayDeque[(Either[Expr, Option[Either[String, Double]]], Boolean)] =
      mutable.ArrayDeque((Left(expr), false))

    while (buffer.nonEmpty) {
      val (elem, traversed) = buffer.removeLast()
      if (traversed) {
        elem match {
          case Right(evaluated) =>
            //   otherwise, remove the two previous elems from the buffer and evaluate the expression, reversing the operands; put the result to the buffer
            if (buffer.isEmpty) {
              return evaluated
            } else if (buffer.size == 1) {
              throw new IllegalStateException("Unexpected buffer size")
            } else {
              val previous = buffer.removeLast()
              if (!previous._2) {
                // If the previous elem is not traversed
                buffer.append((elem, traversed))
                buffer.append(previous)
              } else {
                val previousEvaluated = previous._1.getOrElse(
                  throw new IllegalStateException(s"A value expected, but got an expression: ${previous._1.left}")
                )
                val (exprOrValue, traversed) = buffer.removeLast()
                if (!traversed) {
                  throw new IllegalStateException("Traversed expression expected")
                }
                exprOrValue match {
                  case Right(value) =>
                    throw new IllegalStateException(s"Expression expected, but got value: '$value'")
                  case Left(expr) =>
                    expr match {
                      case NumberValue(v) =>
                        throw new IllegalStateException(s"Expression expected, but got a value expression: $v")
                      case VariableValue(name) =>
                        throw new IllegalStateException(s"Expression expected, but got a variable expression: $name")
                      case _: Addition =>
                        val value = applyOp(evaluated, previousEvaluated, _ + _)
                        buffer.append((Right(value), true))
                      case _: Subtraction =>
                        val value = applyOp(evaluated, previousEvaluated, _ - _)
                        buffer.append((Right(value), true))
                      case _: Multiplication =>
                        val value = applyOp(evaluated, previousEvaluated, _ * _)
                        buffer.append((Right(value), true))
                      case _: Division =>
                        val value = applyOp(evaluated, previousEvaluated, _ / _)
                        buffer.append((Right(value), true))
                    }
                }
              }
            }
          case Left(expr) =>
            throw new IllegalStateException(
              "Traversed expressions should be removed from the buffer when processing traversed values"
            )
        }
      } else {
        elem match {
          case Right(evaluated) =>
            buffer.append((Right(evaluated), true))
          case Left(expr) =>
            expr match {
              case NumberValue(v) =>
                val value: Option[Right[String, Double]] = v.toDoubleOption.map(Right[String, Double])
                buffer.append((Right(value), true))
              case VariableValue(name) =>
                val value: Option[Either[String, Double]] = cellEvaluator.getEvaluatedCellValue(name)
                buffer.append((Right(value), true))
              case Addition(a, b) =>
                buffer.append((Left(expr), true))
                buffer.append((Left(a), false))
                buffer.append((Left(b), false))
              case Subtraction(a, b) =>
                buffer.append((Left(expr), true))
                buffer.append((Left(a), false))
                buffer.append((Left(b), false))
              case Multiplication(a, b) =>
                buffer.append((Left(expr), true))
                buffer.append((Left(a), false))
                buffer.append((Left(b), false))
              case Division(a, b) =>
                buffer.append((Left(expr), true))
                buffer.append((Left(a), false))
                buffer.append((Left(b), false))
            }
        }
      }
    }
    None
  }

  private def applyOp(
      a: Option[Either[String, Double]],
      b: Option[Either[String, Double]],
      op: (Double, Double) => Double
  ): Option[Right[String, Double]] = {
    val res = for {
      a <- a.flatMap(valueToNumber)
      b <- b.flatMap(valueToNumber)
    } yield op(a, b)
    res.map(Right(_))
  }

  private def valueToNumber(value: Either[String, Double]): Option[Double] = value match {
    case Right(value)                 => Some(value)
    case Left(value) if value.isEmpty => Some(0)
    case _                            => None
  }

  def referencedVariables(expr: Expr): Set[String] = {
    val variables: mutable.Builder[String, Set[String]] = Set.newBuilder[String]
    val buffer: mutable.ArrayDeque[Expr]                = mutable.ArrayDeque(expr)

    while (buffer.nonEmpty) {
      val expr = buffer.removeLast()
      expr match {
        case NumberValue(_)       =>
        case VariableValue(name)  => variables += name
        case Addition(a, b)       => buffer.append(a); buffer.append(b)
        case Subtraction(a, b)    => buffer.append(a); buffer.append(b)
        case Multiplication(a, b) => buffer.append(a); buffer.append(b)
        case Division(a, b)       => buffer.append(a); buffer.append(b)
      }
    }

    variables.result()
  }

  // Abstract syntax tree model
  sealed trait Expr

  case class NumberValue(value: String) extends Expr

  case class VariableValue(name: String) extends Expr

  case class Addition(lhs: Expr, rhs: Expr) extends Expr

  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr

  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr

  case class Division(lhs: Expr, rhs: Expr) extends Expr

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
