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

  /** Evaluate a parsed expression
   * @return None on failure, Some otherwise */
  def evaluate(expr: Expr)(implicit cellEvaluator: CellEvaluator): Option[Either[String, Double]] =
    expr match {
      case NumberValue(v)       => v.toDoubleOption.map(Right(_))
      case VariableValue(name)  => cellEvaluator.getEvaluatedCellValue(name)
      case Addition(a, b)       => numberOp(a, b, _ + _)
      case Subtraction(a, b)    => numberOp(a, b, _ - _)
      case Multiplication(a, b) => numberOp(a, b, _ * _)
      case Division(a, b)       => numberOp(a, b, _ / _)
    }

  private def numberOp(a: Expr, b: Expr, op: (Double, Double) => Double)(implicit
      cellEvaluator: CellEvaluator
  ): Option[Right[String, Double]] = {
    val res = for {
      a <- evaluate(a).flatMap(_.toOption)
      b <- evaluate(b).flatMap(_.toOption)
    } yield op(a, b)
    res.map(Right(_))
  }

  def referencedVariables(expr: Expr): Set[String] = {
    val variables: mutable.Builder[String, Set[String]] = Set.newBuilder[String]

    def loop(expr: Expr): Unit = expr match {
      case NumberValue(_)       =>
      case VariableValue(name)  => variables += name
      case Addition(a, b)       => loop(a); loop(b)
      case Subtraction(a, b)    => loop(a); loop(b)
      case Multiplication(a, b) => loop(a); loop(b)
      case Division(a, b)       => loop(a); loop(b)
    }

    loop(expr)
    variables.result()
  }

  // Abstract syntax tree model
  sealed trait Expr

  private case class NumberValue(value: String) extends Expr

  private case class VariableValue(name: String) extends Expr

  private case class Addition(lhs: Expr, rhs: Expr) extends Expr

  private case class Subtraction(lhs: Expr, rhs: Expr) extends Expr

  private case class Multiplication(lhs: Expr, rhs: Expr) extends Expr

  private case class Division(lhs: Expr, rhs: Expr) extends Expr

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
