package pkg

import scala.util.matching.Regex

object StringUtils {

  val whitespaceRegex: Regex = """\s""".r
  // val tokenizerRegex: Regex  = """\b|(?=[+\-*/()])""".r

  def normalize(expr: String): String = whitespaceRegex.replaceAllIn(expr, "").toLowerCase

  // def tokenize(expr: String): Array[String] = tokenizerRegex.split(expr)

}
