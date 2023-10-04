package pkg

import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import pkg.CalcParser.{Addition, Multiplication, NumberValue, Subtraction}
import pkg.Timing.{time, time1}

import scala.util.Random

class SheetSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  private val sheet1: String = "sheet1"

  // Testing performance with a DB would give different results
  val testWithDb             = false
  private var db: Option[Db] = None

  override protected def beforeEach(): Unit = {
    db.foreach(_.close())
    if (testWithDb) {
      db = Some(new Db("volume/db_sheet_spec", recreate = true))
    }
  }

  override protected def afterAll(): Unit =
    db.foreach(_.close())

  "Sheet" should "work correctly" in {
    val sheet: Sheet = new SheetImpl(sheet1, db)
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

    // An empty existing cell is coerced to 0
    sheet.putCellValue("a1", "") shouldEqual Some(Left(""))
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(1)))

    // A cell with just whitespace isn't coerced to 0
    sheet.putCellValue("a4", " ") shouldEqual Some(Left(" "))
    sheet.putCellValue("a5", "=a4+1") shouldEqual None
    sheet.putCellValue("a5", "=a4") shouldEqual Some(Left(" "))
    sheet.putCellValue("a6", "=a5") shouldEqual Some(Left(" "))

    sheet.putCellValue("a1", "=a3") shouldEqual None
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(1)))
    sheet.getCellValue("a3") shouldEqual None

    // Can't change a1 value type because a2==a1+1
    sheet.putCellValue("a1", "a1") shouldEqual None
    sheet.getCellValue("a1") shouldEqual Some(("", Left("")))
    sheet.getCellValue("a2") shouldEqual Some(("=a1+1", Right(1)))

    sheet.putCellValue("a1", "=((123+456*(2+-1))+789)/0.1") shouldEqual Some(Right(13680))
    sheet.putCellValue("a1", "=123e-4   +   56e7 + 8.9e10 + .12e+3 + 4e5 + 6e0") shouldEqual
    Some(Right(8.95604001260123e10))
    sheet.putCellValue("a1", "1e150") shouldEqual Some(Right(1.0e150))
    sheet.putCellValue("a1", "=1e150") shouldEqual Some(Right(1.0e150))

    // One or both sides of whitespace should be operators
    sheet.putCellValue("a1", "=123 456") shouldEqual None
    sheet.putCellValue("a3", "=a1 a2") shouldEqual None

    sheet.putCellValue("a2", "a2") shouldEqual Some(Left("a2"))
    sheet.getCellValue("a2") shouldEqual Some("a2", Left("a2"))
    sheet.putCellValue("a1", "=a2") shouldEqual Some(Left("a2"))
    sheet.getCellValue("a1") shouldEqual Some("=a2", Left("a2"))

    an[IllegalArgumentException] shouldBe thrownBy {
      sheet.getCellValue("")
      sheet.putCellValue("", "123")
    }

    sheet.getCellValues shouldEqual Map(
      "a1" -> ("=a2", Left("a2")),
      "a2" -> ("a2", Left("a2")),
      "a4" -> (" ", Left(" ")),
      "a5" -> ("=a4", Left(" ")),
      "a6" -> ("=a5", Left(" "))
    )
  }

  it should "check topological sorting" in {
    val sheet: SheetImpl = new SheetImpl(sheet1, db)
    sheet.putCellValue("a", "1") shouldEqual Some(Right(1))
    sheet.putCellValue("b", "=a+1") shouldEqual Some(Right(2))
    sheet.putCellValue("c", "=b+a") shouldEqual Some(Right(3))
    sheet.putCellValue("d", "=c+b+1") shouldEqual Some(Right(6))
    sheet.putCellValue("e", "=c+d+1") shouldEqual Some(Right(10))

    sheet.putCellValue("c", "=d") shouldEqual None
    sheet.putCellValue("a", "=e") shouldEqual None

    sheet.putCellValue("c", "=b+10") shouldEqual Some(Right(12))

    sheet.allTopCellsTopologicallySorted(sheet.getCell("c").get).map(_.name) shouldEqual Seq("d", "e")
    sheet.allTopCellsTopologicallySorted(sheet.getCell("e").get).map(_.name) shouldEqual Seq.empty
    sheet.allTopCellsTopologicallySorted(sheet.getCell("a").get).map(_.name) shouldEqual Seq("b", "c", "d", "e")

    sheet.getCellValue("a") shouldEqual Some(("1", Right(1)))
    sheet.getCellValue("b") shouldEqual Some(("=a+1", Right(2)))
    sheet.getCellValue("c") shouldEqual Some(("=b+10", Right(12)))
    sheet.getCellValue("d") shouldEqual Some(("=c+b+1", Right(15)))
    sheet.getCellValue("e") shouldEqual Some(("=c+d+1", Right(28)))

    sheet.putCellValue("f", "=c+10") shouldEqual Some(Right(22))
    sheet.putCellValue("g", "=f+1") shouldEqual Some(Right(23))

    val sorted1 = sheet.allTopCellsTopologicallySorted(sheet.getCell("a").get).map(_.name)
    sorted1.indexOf("b") should be < sorted1.indexOf("c")
    sorted1.indexOf("c") should be < sorted1.indexOf("d")
    sorted1.indexOf("c") should be < sorted1.indexOf("e")
    sorted1.indexOf("d") should be < sorted1.indexOf("e")
    sorted1.indexOf("c") should be < sorted1.indexOf("f")
    sorted1.indexOf("f") should be < sorted1.indexOf("g")

    sheet.allTopCellsTopologicallySorted(sheet.getCell("f").get).map(_.name) shouldEqual Seq("g")
    sheet.allTopCellsTopologicallySorted(sheet.getCell("g").get).map(_.name) shouldEqual Seq.empty

    sheet.getCellValue("a") shouldEqual Some(("1", Right(1)))
    sheet.getCellValue("b") shouldEqual Some(("=a+1", Right(2)))
    sheet.getCellValue("c") shouldEqual Some(("=b+10", Right(12)))
    sheet.getCellValue("d") shouldEqual Some(("=c+b+1", Right(15)))
    sheet.getCellValue("e") shouldEqual Some(("=c+d+1", Right(28)))
    sheet.getCellValue("f") shouldEqual Some(("=c+10", Right(22)))
    sheet.getCellValue("g") shouldEqual Some(("=f+1", Right(23)))

    sheet.putCellValue("b", "=a+2") shouldEqual Some(Right(3))

    sheet.getCellValue("a") shouldEqual Some(("1", Right(1)))
    sheet.getCellValue("b") shouldEqual Some(("=a+2", Right(3)))
    sheet.getCellValue("c") shouldEqual Some(("=b+10", Right(13)))
    sheet.getCellValue("d") shouldEqual Some(("=c+b+1", Right(17)))
    sheet.getCellValue("e") shouldEqual Some(("=c+d+1", Right(31)))
    sheet.getCellValue("f") shouldEqual Some(("=c+10", Right(23)))
    sheet.getCellValue("g") shouldEqual Some(("=f+1", Right(24)))

    sheet.putCellValue("g", "=f+d+1") shouldEqual Some(Right(41))
  }

  it should "check a long formula chain" in {
    val sheet: SheetImpl = new SheetImpl(sheet1, db)
    sheet.putCellValue("a1", "1") shouldEqual Some(Right(1))
    val n = 1e6.toInt
    time1("Create a long formula chain")(
      (2 to n).foreach(i => sheet.putCellValue(s"a$i", s"=a${i - 1}+1") shouldEqual Some(Right(i)))
    )
    time1("Update the top value")(
      sheet.putCellValue(s"a$n", s"=a${n - 2}+3") shouldEqual Some(Right(n + 1))
    )
    time1("Update the bottom value")(
      sheet.putCellValue(s"a1", s"=2") shouldEqual Some(Right(2))
    )
    sheet.getCellValue(s"a${n / 2}") shouldEqual Some(s"=a${n / 2 - 1}+1", Right(n / 2 + 1))
    time1("Update a value in the middle")(
      sheet.putCellValue(s"a${n / 2}", s"=a${n / 2 - 2}+3") shouldEqual Some(Right(n / 2 + 2))
    )
    sheet.getCellValue(s"a$n") shouldEqual Some(s"=a${n - 2}+3", Right(n + 3))
  }

  it should "calculate an expression" in {
    val sheet = new SheetImpl(sheet1, db)
    val expr = Multiplication(
      Addition(
        NumberValue("1"),
        NumberValue("2")
      ),
      Subtraction(
        NumberValue("3"),
        NumberValue("4")
      )
    )
    CalcParser.evaluate(expr)(sheet) shouldEqual Some(Right(-3))
  }

  it should "check a long formula" in {
    val sheet: SheetImpl = new SheetImpl(sheet1, db)
    val n                = 1e6.toInt
    val formula          = "+1" * n
    time1("Calculate a long formula")(
      sheet.putCellValue("a1", s"=$formula") shouldEqual Some(Right(n))
    )
  }

  it should "measure performance" in {
    time("Simple value", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl(sheet1, db)
      sheet.putCellValue("a1", "1")
      sheet.getCellValue("a1")
    }

    time("Simple expression", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl(sheet1, db)
      sheet.putCellValue("a1", "=1")
      sheet.getCellValue("a1")
    }

    time("Simple expression utf", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl(sheet1, db)
      sheet.putCellValue("Ａ1", "=1")
      sheet.getCellValue("Ａ1")
    }

    {
      val iterations = 1e6.toInt
      val data       = (1 to iterations).map(_ => ("a" + Random.alphanumeric(5), Random.nextInt.toString)).toArray.iterator
      time("Various simple values", iterations) {
        val sheet: Sheet  = new SheetImpl(sheet1, db)
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
        val sheet: Sheet  = new SheetImpl(sheet1, db)
        val (name, value) = data.next()
        sheet.putCellValue(name, value)
        sheet.getCellValue(name)
      }
    }

    time("Complex expression", 1e6.toInt) {
      val sheet: Sheet = new SheetImpl(sheet1, db)
      sheet.putCellValue("a1", "=((123+456*(2+-1))+789)/0.1")
      sheet.getCellValue("a1")
    }
  }

}
