package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
case class Items(
    items: Seq[Item]
)

case class Item(
    name: String,
    coordinates: Coordinates,
    `type`: Type,
    address: String,
    schedule: Option[String],
    accessibility: Seq[AccessibilityItem],
    source: String,
    phone: String
)

case class Type(
    name: String,
    label: String
)

case class AccessibilityItem(
    name: String,
    label: String
)

object Json {
  implicit val ItemCodec: JsonValueCodec[Item]   = JsonCodecMaker.make
  implicit val ItemsCodec: JsonValueCodec[Items] = JsonCodecMaker.make
}
