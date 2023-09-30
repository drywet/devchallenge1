package pkg

import io.activej.eventloop.Eventloop
import io.activej.http.HttpMethod.GET
import io.activej.http.{AsyncHttpServer, HttpRequest, HttpResponse, RoutingServlet}

object Main {

  // TODO http service + docker-compose + load test with k6

  private val servlet = RoutingServlet.create
    .map(
      GET,
      "/",
      (request: HttpRequest) =>
        HttpResponse.ok200.withHtml(
          "<h1>Go to some pages</h1>" + "<a href=\"/path1\"> Path 1 </a><br>" + "<a href=\"/path2\"> Path 2 </a><br>" + "<a href=\"/user/0\"> Data for user with ID 0 </a><br>" + "<br>" + "<a href=\"/path3\"> Non existent </a>"
        )
    )
    .map(
      GET,
      "/path1",
      (request: HttpRequest) =>
        HttpResponse.ok200.withHtml("<h1>Hello from the first path!</h1>" + "<a href=\"/\">Go home</a>")
    )
    .map(
      GET,
      "/path2",
      (request: HttpRequest) =>
        HttpResponse.ok200.withHtml("<h1>Hello from the second path!</h1>" + "<a href=\"/\">Go home</a>")
    )
    .map(
      GET,
      "/user/:user_id",
      (request: HttpRequest) => {
        val userId = request.getPathParameter("user_id")
        HttpResponse.ok200.withHtml(
          "<h1>You have requested data for user with ID: " + userId + "</h1>" + "<h3>Try changing URL after <i>'.../user/'</i> to get data for users with different IDs</h3>"
        )

      }
    )
    .map(
      "/*",
      (request: HttpRequest) =>
        HttpResponse
          .ofCode(404)
          .withHtml("<h1>404</h1><p>Path '" + request.getRelativePath + "' not found</p>" + "<a href=\"/\">Go home</a>")
    )

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
