package pkg

import org.parboiled2._

object CalcParser {

  def evaluate(expr: Expr)(implicit cellEvaluator: CellEvaluator): Option[Double] =
    expr match {
      case NumberValue(v)       => v.toDoubleOption
      case VariableValue(name)  => cellEvaluator.getAndEvaluateCellAsNumber(name)
      case Addition(a, b)       => evaluate(a).flatMap(a => evaluate(b).map(b => a + b))
      case Subtraction(a, b)    => evaluate(a).flatMap(a => evaluate(b).map(b => a - b))
      case Multiplication(a, b) => evaluate(a).flatMap(a => evaluate(b).map(b => a * b))
      case Division(a, b)       => evaluate(a).flatMap(a => evaluate(b).map(b => a / b))
    }

  // our abstract syntax tree model
  sealed trait Expr

  case class NumberValue(value: String) extends Expr

  case class VariableValue(name: String) extends Expr

  case class Addition(lhs: Expr, rhs: Expr) extends Expr

  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr

  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr

  case class Division(lhs: Expr, rhs: Expr) extends Expr

}

/** This parser reads simple calculator expressions and builds an AST
 * for them, to be evaluated in a separate phase, after parsing is completed.
 */
class CalcParser(val input: ParserInput) extends Parser {

  import CalcParser._

  // val opsPredicate: CharPredicate = CharPredicate('+', '-', '*', '/', '(', ')')

  def InputLine = rule(Expression ~ EOI)

  def Expression: Rule1[Expr] =
    rule {
      Term ~ zeroOrMore(
        '+' ~ Term ~> Addition.apply _
          | '-' ~ Term ~> Subtraction.apply _
      )
    }

  def Term =
    rule {
      Factor ~ zeroOrMore(
        '*' ~ Factor ~> Multiplication.apply _
          | '/' ~ Factor ~> Division.apply _
      )
    }

  def Factor = rule(Number | Variable | Parens)

  def Parens = rule('(' ~ Expression ~ ')')

  def Number = rule(capture(Float) ~> NumberValue.apply _)

  def Float = rule(
    optional(CharPredicate('+', '-')) ~ FloatDigits ~ optional(FloatExponent)
  )

  def FloatDigits = rule(
    (oneOrMore(CharPredicate.Digit) ~ optional(CharPredicate('.')) ~ zeroOrMore(CharPredicate.Digit)) |
      (CharPredicate('.') ~ oneOrMore(CharPredicate.Digit))
  )

  def FloatExponent = rule(CharPredicate('e') ~ optional(CharPredicate('+', '-')) ~ oneOrMore(CharPredicate.Digit))

  def Variable =
    // No support for variables starting with a digit, because `1e1` should be a number, not a variable
    rule(capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> VariableValue.apply _)

}
