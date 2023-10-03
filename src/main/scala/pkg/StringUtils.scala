package pkg

import scala.util.matching.Regex

object StringUtils {

  private val whitespaceRegex: Regex = """\s+""".r
  private val operators              = Set('+', '-', '*', '/', '(', ')')

  /** Remove whitespace making sure no adjacent numbers or variables would join together */
  def normalizeFormula(expr: String): Option[String] = {
    // Minimize whitespace
    val a = whitespaceRegex.replaceAllIn(expr, " ").trim
    // Either side of whitespace should be an operator
    (1 until (a.length - 1)).foreach { i =>
      if (a(i) == ' ') {
        if (!(operators(a(i - 1)) || operators(a(i + 1))))
          return None
      }
    }
    // Remove whitespace
    Some(whitespaceRegex.replaceAllIn(a, ""))
  }

  def normalizeId(id: String): Option[String] = {
    // Minimize whitespace
    val a = whitespaceRegex.replaceAllIn(id, " ").trim
    if (!a.contains(' ')) Some(a.toLowerCase) else None
  }

}
