package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.tototoshi.csv._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pkg.Json._

import java.io.File
import java.nio.file.{Files, Paths}

case class Coordinates(latitude: String, longitude: String)

class CsvSpec extends AnyFlatSpec with should.Matchers {

  val accessibilityItems: IndexedSeq[(String, String)] = IndexedSeq(
    "SPECIAL_PARKING"          -> "Спеціальні місця для паркування",
    "SPECIAL_ENTRANCE"         -> "Вхідна група",
    "NON_VISUAL_ORIENTATION"   -> "Засоби невізуальної орієнтації",
    "NON_ACOUSTIC_ORIENTATION" -> "Засоби неакустичної орієнтації",
    "ELEVATOR"                 -> "Наявність спеціального ліфту",
    "SPECIAL_WC"               -> "Санітарно-гігєнічні приміщення",
    "SPECIAL_NOTIFICATION"     -> "Адаптовані пристрої оповіщення",
    "GESTURE_TRANSLATOR"       -> "Перекладач з жестової мови",
    "SHELTER"                  -> "Укриття",
    "WI_FI"                    -> "Wi-Fi",
    "CHANGING_TABLE"           -> "Пеленальний столик",
    "CHILD_ROOM"               -> "Дитяча кімната",
    "ACCOMPANYING_PERSON"      -> "Супроводжуюча людина"
  )

  "it" should "parse csv and store to json" in {
    val reader                  = CSVReader.open(new File("D:/a/spring1/csvparser/мапа доступності.xlsx - Аркуш1.csv"))
    val table: Seq[Seq[String]] = reader.all().drop(1)
    val items = table
      .map(item =>
        Item(
          name = item(2),
          coordinates = Coordinates(latitude = item(27), longitude = item(28)),
          `type` = Type(name = "INFRASTRUCTURE", label = "Інфраструктура"),
          address = item.slice(23, 27).filter(_.trim.nonEmpty).mkString(", "),
          schedule = None,
          accessibility = item
            .slice(3, 16)
            .zipWithIndex
            .filter(_._1.toLowerCase.contains("так"))
            .map(x => accessibilityItems(x._2))
            .map { case (name, label) => AccessibilityItem(name = name, label = label) },
          source = "Мапа доступності",
          phone = Seq(item(16), item(17)).filter(_.trim.nonEmpty).mkString("; ")
        )
      )
    val json = writeToString(Items(items))
    Files.writeString(Paths.get("objects.json"), json)
  }

}
