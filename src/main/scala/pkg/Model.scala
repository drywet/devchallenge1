package pkg

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

object Model {

  val Ok       = """{"status": "ok"}"""
  val NotFound = """{"status": "error", "error":"not_found"}"""

  case class PostCellRequest(value: String)
  case class PostCellResponse(value: String, result: String)
  case class GetCellResponse(value: String, result: String)
  case class GetSheetResponseItem(value: String, result: String)

  implicit val PostCellRequestCodec: JsonValueCodec[PostCellRequest]                    = JsonCodecMaker.make
  implicit val PostCellResponseCodec: JsonValueCodec[PostCellResponse]                  = JsonCodecMaker.make
  implicit val GetCellResponseCodec: JsonValueCodec[GetCellResponse]                    = JsonCodecMaker.make
  implicit val GetSheetResponseCodec: JsonValueCodec[Map[String, GetSheetResponseItem]] = JsonCodecMaker.make

}
