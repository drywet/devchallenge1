package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import io.activej.bytebuf.ByteBuf
import io.activej.http.HttpMethod.{GET, POST}
import io.activej.http._
import org.slf4j.{Logger, LoggerFactory}
import pkg.Model.{GetCellResponse, GetSheetResponseCodec, GetSheetResponseItem, NotFound, Ok, PostCellRequest, PostCellResponse, SubscribeToCellRequest, SubscribeToCellResponse, evaluatedResultFormat}

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Try

class HttpService(db: Option[Db], httpClient: AsyncHttpClient) {

  val Error: String = "ERROR"

  private val maxPayloadSize: Int = 100 * 1024 * 1024

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  private val service        = new Service(db, httpClient)

  private val postCell: AsyncServlet = { request: HttpRequest =>
    request.loadBody(maxPayloadSize).`then` { (requestBody: ByteBuf) =>
      val sheetId        = request.getPathParameter("sheet_id")
      val cellId         = request.getPathParameter("cell_id")
      val sourceValueTry = Try(readFromString[PostCellRequest](requestBody.asString(UTF_8)).value)
      try {
        val sourceValue = sourceValueTry.get
        service
          .putCell(sheetId, cellId, sourceValue)
          .map { result =>
            val jsonStr = writeToString(PostCellResponse(value = sourceValue, result = evaluatedResultFormat(result)))
            HttpResponse.ok201.withJson(jsonStr).promise()
          }
          .getOrElse {
            val jsonStr = writeToString(PostCellResponse(value = sourceValue, result = Error))
            HttpResponse.ofCode(422).withJson(jsonStr).promise()
          }
      } catch {
        case e: Throwable =>
          logger.warn(s"Exception: $e")
          val jsonStr = writeToString(PostCellResponse(value = sourceValueTry.getOrElse(""), result = Error))
          HttpResponse.ofCode(422).withJson(jsonStr).promise()
      }
    }
  }

  private val getCell: AsyncServlet = { request: HttpRequest =>
    val sheetId = request.getPathParameter("sheet_id")
    val cellId  = request.getPathParameter("cell_id")
    try {
      service
        .getCell(sheetId, cellId)
        .map { case (sourceValue, result) =>
          val jsonStr = writeToString(GetCellResponse(value = sourceValue, result = evaluatedResultFormat(result)))
          HttpResponse.ok200.withJson(jsonStr).promise()
        }
        .getOrElse(HttpResponse.ofCode(404).withJson(NotFound).promise())
    } catch {
      case e: Throwable =>
        logger.warn(s"Exception: $e")
        HttpResponse.ofCode(404).withJson(NotFound).promise()
    }
  }

  private val subscribeToCell: AsyncServlet = { request: HttpRequest =>
    request.loadBody(maxPayloadSize).`then` { (requestBody: ByteBuf) =>
      val sheetId       = request.getPathParameter("sheet_id")
      val cellId        = request.getPathParameter("cell_id")
      val webhookUrlTry = Try(readFromString[SubscribeToCellRequest](requestBody.asString(UTF_8)).webhook_url)
      try {
        val webhookUrl = webhookUrlTry.get
        service
          .subscribeToCell(sheetId, cellId, webhookUrl)
          .map { _ =>
            val jsonStr = writeToString(SubscribeToCellResponse(webhook_url = webhookUrl))
            HttpResponse.ok201.withJson(jsonStr).promise()
          }
          .getOrElse {
            val jsonStr = writeToString(SubscribeToCellResponse(webhook_url = webhookUrl))
            HttpResponse.ofCode(422).withJson(jsonStr).promise()
          }
      } catch {
        case e: Throwable =>
          logger.warn(s"Exception: $e")
          val jsonStr = writeToString(SubscribeToCellResponse(webhook_url = webhookUrlTry.getOrElse("")))
          HttpResponse.ofCode(422).withJson(jsonStr).promise()
      }
    }
  }

  private val getSheet: AsyncServlet = { request: HttpRequest =>
    val sheetId = request.getPathParameter("sheet_id")
    try {
      service
        .getSheet(sheetId)
        .map { sheet =>
          val sheetResponse: Map[String, GetSheetResponseItem] = sheet.view.mapValues { case (sourceValue, result) =>
            GetSheetResponseItem(value = sourceValue, result = evaluatedResultFormat(result))
          }.toMap
          val jsonStr = writeToString(sheetResponse)(GetSheetResponseCodec)
          HttpResponse.ok200.withJson(jsonStr).promise()
        }
        .getOrElse(HttpResponse.ofCode(404).withJson(NotFound).promise())
    } catch {
      case e: Throwable =>
        logger.warn(s"Exception: $e")
        HttpResponse.ofCode(404).withJson(NotFound).promise()
    }
  }

  val servlet: RoutingServlet = RoutingServlet.create
    .map(GET, "/", (_: HttpRequest) => HttpResponse.ok200.withJson(Ok))
    .map(POST, "/api/v1/:sheet_id/:cell_id/subscribe", subscribeToCell)
    .map(POST, "/api/v1/:sheet_id/:cell_id", postCell)
    .map(GET, "/api/v1/:sheet_id/:cell_id", getCell)
    .map(GET, "/api/v1/:sheet_id", getSheet)
    .map("/*", (_: HttpRequest) => HttpResponse.notFound404().withJson(NotFound))

}
