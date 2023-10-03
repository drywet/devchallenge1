package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import io.activej.bytebuf.ByteBuf
import io.activej.http.HttpMethod.{GET, POST}
import io.activej.http._
import pkg.DoubleUtils.cellDoubleFormat
import pkg.Model.{
  GetCellResponse,
  GetSheetResponseCodec,
  GetSheetResponseItem,
  NotFound,
  Ok,
  PostCellRequest,
  PostCellResponse
}

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Try

// TODO Increase stack size and add tests for long deps chains updates at the bottom and at the top

// TODO Store state in a volume

// TODO docker-compose

// TODO document

// TODO load test with k6

// TODO Optimization:
//  replace PEG/AST parser with RPN?
//  replace sets with ArraySeq where possible?

class HttpService {

  val Error: String = "ERROR"

  private val maxPayloadSize: Int = 100 * 1024 * 1024
  private val debug               = false

  private val service = new Service()

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
          if (debug) println(s"Exception: $e")
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
        if (debug) println(s"Exception: $e")
        HttpResponse.ofCode(404).withJson(NotFound).promise()
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
        if (debug) println(s"Exception: $e")
        HttpResponse.ofCode(404).withJson(NotFound).promise()
    }
  }

  private def evaluatedResultFormat(value: Either[String, Double]): String = value match {
    case Right(value) => cellDoubleFormat(value)
    case Left(value)  => value
  }

  val servlet: RoutingServlet = RoutingServlet.create
    .map(GET, "/", (_: HttpRequest) => HttpResponse.ok200.withJson(Ok))
    .map(POST, "/api/v1/:sheet_id/:cell_id", postCell)
    .map(GET, "/api/v1/:sheet_id/:cell_id", getCell)
    .map(GET, "/api/v1/:sheet_id", getSheet)
    .map("/*", (_: HttpRequest) => HttpResponse.notFound404().withJson(NotFound))

}
