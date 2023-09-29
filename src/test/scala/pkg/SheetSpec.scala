package pkg

import org.scalatest.flatspec._
import org.scalatest.matchers._
import pkg.DoubleUtils.scientificFormat

import scala.util.Random

class SheetSpec extends AnyFlatSpec with should.Matchers {

  "Sheet" should "work correctly in sequential fashion" in {
    val sheet: Sheet = new SheetImpl()
    sheet.getCellValue("a1") shouldEqual None
    sheet.getCellValue("a1") shouldEqual None
    sheet.getCellValue("1") shouldEqual None
    sheet.getCellValue("1") shouldEqual None
    sheet.putCellValue("a1", "1") shouldEqual Some(Right(1))
    sheet.getCellValue("a1") shouldEqual Some(("1", Right(1)))
    sheet.putCellValue("a2", "=a1+1") shouldEqual Some(Right(2))
    sheet.putCellValue("a1", "3") shouldEqual Some(Right(3))
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(4)))

    sheet.putCellValue("a1", "=a2") shouldEqual None
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(4)))
    sheet.putCellValue("a1", "5") shouldEqual Some(Right(5))
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(6)))

    sheet.putCellValue("a1", "") shouldEqual None
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(6)))

    sheet.putCellValue("a1", "=a3") shouldEqual None
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(6)))
    sheet.getCellValue("a3") shouldEqual None

    sheet.putCellValue("a1", "a1") shouldEqual None
    sheet.getCellValue("a1") shouldEqual Some(("5", Right(5)))
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(6)))

    sheet.putCellValue("a1", "=((123+456*(2+-1))+789)/0.1") shouldEqual Some(Right(13680))
    sheet.putCellValue("a1", "=123e-4 + 56e7 + 8.9e10 + .12e+3 + 4e5 + 6e0") shouldEqual
    Some(Right(8.95604001260123e10))
    sheet.putCellValue("a1", "1e150") shouldEqual Some(Right(1.0e150))
    sheet.putCellValue("a1", "=1e150") shouldEqual Some(Right(1.0e150))

    sheet.putCellValue("a2", "a2") shouldEqual Some(Left("a2"))
    sheet.getCellValue("a2") shouldEqual Some("a2", Left("a2"))

    sheet.putCellValue("a1", "=a2") shouldEqual Some(Left("a2"))
    sheet.getCellValue("a1") shouldEqual Some("=a2", Left("a2"))

    an[IllegalArgumentException] shouldBe thrownBy {
      sheet.getCellValue("")
      sheet.putCellValue("", "123")
    }
  }

  it should "perf test" in {
    time("Simple value", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl()
      sheet.putCellValue("a1", "1")
      sheet.getCellValue("a1")
    }

    time("Simple expression", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl()
      sheet.putCellValue("a1", "=1")
      sheet.getCellValue("a1")
    }

    {
      val iterations = 1e6.toInt
      val data       = (1 to iterations).map(_ => ("a" + Random.alphanumeric(5), Random.nextInt.toString)).toArray.iterator
      time("Various simple values", iterations) {
        val sheet: Sheet  = new SheetImpl()
        val (name, value) = data.next()
        sheet.putCellValue(name, value)
        sheet.getCellValue(name)
      }
    }

    {
      val iterations = 1e6.toInt
      val data =
        (1 to iterations).map(_ => ("a" + Random.alphanumeric(5), s"=${Random.nextInt.toString}")).toArray.iterator
      time("Various simple expressions", iterations) {
        val sheet: Sheet  = new SheetImpl()
        val (name, value) = data.next()
        sheet.putCellValue(name, value)
        sheet.getCellValue(name)
      }
    }

    time("Complex expression", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl()
      sheet.putCellValue("a1", "=((123+456*(2+-1))+789)/0.1")
      sheet.getCellValue("a1")
    }
  }

  private def time(name: String, iterations: Int)(f: => Unit): Unit = {
    val t0 = System.nanoTime()
    (1 to iterations).foreach(_ => f)
    val t1      = System.nanoTime()
    val seconds = (t1 - t0) / 1e9
    val rps     = scientificFormat(iterations / seconds)
    println(f"$name%30s: \trps: $rps, \tseconds: ${Math.round(seconds * 10) / 10.0}")
  }

}
