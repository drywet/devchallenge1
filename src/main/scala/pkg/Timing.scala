package pkg

import org.slf4j.{Logger, LoggerFactory}
import pkg.DoubleUtils.scientificFormat

object Timing {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def time(name: String, iterations: Int)(f: => Unit): Unit = {
    val t0 = System.nanoTime()
    (1 to iterations).foreach(_ => f)
    val t1      = System.nanoTime()
    val seconds = (t1 - t0) / 1e9
    val rps     = scientificFormat(iterations / seconds)
    logger.warn(f"$name%30s: \trps: $rps, \tseconds: ${Math.round(seconds * 10) / 10.0}")
  }

  def time1[A](name: String)(f: => A): A = {
    val t0      = System.nanoTime()
    val result  = f
    val t1      = System.nanoTime()
    val seconds = (t1 - t0) / 1e9
    val rps     = scientificFormat(1 / seconds)
    logger.warn(f"$name%30s: \trps: $rps, \tseconds: ${Math.round(seconds * 10) / 10.0}")
    result
  }

}
