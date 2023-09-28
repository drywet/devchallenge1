package pkg

import java.math.RoundingMode

object DoubleUtils {

  def cellDoubleFormat(a: Double): String = {
    val str = a.toString
    if (str.endsWith(".0")) str.dropRight(2) else str
  }

  def scientificFormat(a: Double): String = {
    val n = BigDecimal(a).bigDecimal
    n.setScale(n.scale() - n.precision() + 3, RoundingMode.HALF_UP).toString
  }

}
