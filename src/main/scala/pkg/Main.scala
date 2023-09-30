package pkg

import io.activej.eventloop.Eventloop
import io.activej.http.HttpMethod.{GET, POST}
import io.activej.http._
import pkg.DoubleUtils.cellDoubleFormat

// TODO http service + docker-compose + load test with k6

object Main {

  val Error         = "ERROR"
  private val debug = false

  private val service = new Service()

  private val postCell: AsyncServlet = { request: HttpRequest =>
    // TODO Is this a worker thread or an IO thread?
    val sheetId = request.getPathParameter("sheet_id")
    val cellId  = request.getPathParameter("cell_id")
    // TODO Parse json
    val sourceValue = request.getPostParameter("value")
    try {
      service.putCell(sheetId, cellId, sourceValue) match {
        case Some(Right(value)) =>
          val valueStr = cellDoubleFormat(value)
          // TODO Escape json strings
          HttpResponse.ok201.withJson(s"""{"value": "$sourceValue", "result": "$valueStr"}""")
        case Some(Left(value)) =>
          // TODO Escape json strings
          HttpResponse.ok201.withJson(s"""{"value": "$sourceValue", "result": "$value"}""")
        case None =>
          HttpResponse.ofCode(422).withJson(s"""{"value": "$sourceValue", "result": "$Error"}""")
      }
    } catch {
      case e: Throwable =>
        if (debug) println(s"Exception: $e")
        HttpResponse.ofCode(422).withJson(s"""{"value": "$sourceValue", "result": "$Error"}""")
    }
  }

  private val servlet = RoutingServlet.create
    .map(GET, "/", (_: HttpRequest) => HttpResponse.ok200.withJson("""{"status": "ok"}"""))
    .map(POST, "/api/v1/:sheet_id/:cell_id", postCell)
    .map("/*", (_: HttpRequest) => HttpResponse.notFound404().withJson("""{"status": "error", "error": "not_found"}"""))

  def main(args: Array[String]): Unit = {
    val eventloop = Eventloop.create()
    val server = AsyncHttpServer
      .create(eventloop, servlet)
      .withListenPort(8080)
    server.listen()
    System.out.println("Server is running")
    System.out.println("You can connect from browser by visiting 'http://localhost:8080/'")
    eventloop.run()
  }

}
