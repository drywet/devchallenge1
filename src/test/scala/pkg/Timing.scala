package pkg

import pkg.DoubleUtils.scientificFormat

object Timing {

  def time(name: String, iterations: Int)(f: => Unit): Unit = {
    val t0 = System.nanoTime()
    (1 to iterations).foreach(_ => f)
    val t1      = System.nanoTime()
    val seconds = (t1 - t0) / 1e9
    val rps     = scientificFormat(iterations / seconds)
    println(f"$name%30s: \trps: $rps, \tseconds: ${Math.round(seconds * 10) / 10.0}")
  }

}
