package pkg

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import pkg.DoubleUtils.cellDoubleFormat

object Model {

  val Ok       = """{"status": "ok"}"""
  val NotFound = """{"status": "error", "error":"not_found"}"""

  case class PostCellRequest(value: String)
  case class PostCellResponse(value: String, result: String)
  case class GetCellResponse(value: String, result: String)
  case class GetSheetResponseItem(value: String, result: String)
  case class SubscribeToCellRequest(webhook_url: String)
  case class SubscribeToCellResponse(webhook_url: String)
  case class SubscribeToCellWebhookRequest(value: String, result: String)

  case class DbKey(sheetId: String, cellId: String)
  case class DbValue(value: String)
  case class DbItemParsed(
      cellId: String,
      value: String,
      referencedVariables: Seq[String],
      var traversed: Boolean
  )

  implicit val PostCellRequestCodec: JsonValueCodec[PostCellRequest]                             = JsonCodecMaker.make
  implicit val PostCellResponseCodec: JsonValueCodec[PostCellResponse]                           = JsonCodecMaker.make
  implicit val GetCellResponseCodec: JsonValueCodec[GetCellResponse]                             = JsonCodecMaker.make
  implicit val GetSheetResponseCodec: JsonValueCodec[Map[String, GetSheetResponseItem]]          = JsonCodecMaker.make
  implicit val SubscribeToCellRequestCodec: JsonValueCodec[SubscribeToCellRequest]               = JsonCodecMaker.make
  implicit val SubscribeToCellResponseCodec: JsonValueCodec[SubscribeToCellResponse]             = JsonCodecMaker.make
  implicit val SubscribeToCellWebhookRequestCodec: JsonValueCodec[SubscribeToCellWebhookRequest] = JsonCodecMaker.make

  implicit val DbKeyCodec: JsonValueCodec[DbKey]     = JsonCodecMaker.make
  implicit val DbValueCodec: JsonValueCodec[DbValue] = JsonCodecMaker.make

  sealed trait Pass
  case object ForwardPass  extends Pass
  case object BackwardPass extends Pass

  case class CellUpdate(
      cellId: String,
      webhookUrl: String,
      sourceValue: String,
      result: Either[String, Double]
  )

  def evaluatedResultFormat(value: Either[String, Double]): String = value match {
    case Right(value) => cellDoubleFormat(value)
    case Left(value)  => value
  }

}
