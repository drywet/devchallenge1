package pkg

import io.activej.bytebuf.ByteBufStrings.encodeAscii
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ScheduledThreadPoolExecutor
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.Using

class AppHttpServerLauncherSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfterAll {

  val port              = 8082
  val serverWorkerCount = 4

  implicit val executionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(new ScheduledThreadPoolExecutor(10))

  val launcher = new AppHttpServerLauncher(port, serverWorkerCount, "volume/db_app_http_server_launcher_spec", true)

  override protected def beforeAll(): Unit = {
    Future(launcher.launch(Array.empty))
    Await.ready(launcher.getStartFuture.asScala, 10.seconds)
  }

  override protected def afterAll(): Unit = {
    launcher.close()
    executionContext.shutdown()
  }

  "AppHttpServerLauncher" should "work correctly" in {
    Using.Manager { use =>
      val socket1   = use(new Socket())
      val socket2   = use(new Socket())
      val localhost = new InetSocketAddress("localhost", port)
      socket1.connect(localhost)
      socket2.connect(localhost)
      val res1 = Future(test(socket = socket1, sheetId = "sheet1", cellId = "cell1"))
      val res2 = Future(test(socket = socket2, sheetId = "sheet2", cellId = "cell2"))
      Await.result(Future.sequence(Seq(res1, res2)), 10.seconds)
    }.get
  }

  private def test(socket: Socket, sheetId: String, cellId: String) = {
    val request =
      s"""POST /api/v1/%20${sheetId.toUpperCase}%20/%20${cellId.toUpperCase}%20 HTTP/1.1
         |Host: localhost
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 16
         |
         |{"value":"=1+2"}GET /api/v1/$sheetId/$cellId HTTP/1.1
         |Host: localhost
         |Connection: keep-alive
         |
         |GET /api/v1/$sheetId HTTP/1.1
         |Host: localhost
         |Connection: keep-alive
         |
         |POST /api/v1/$sheetId/$cellId HTTP/1.1
         |Host: localhost
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 3
         |
         |!@#""".stripMargin.replace("\r\n", "\n").replace("\n", "\r\n")
    socket.getOutputStream.write(encodeAscii(request))
    val expectedResponse =
      s"""HTTP/1.1 201 Created
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 29
         |
         |{"value":"=1+2","result":"3"}HTTP/1.1 200 OK
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 29
         |
         |{"value":"=1+2","result":"3"}HTTP/1.1 200 OK
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 39
         |
         |{"$cellId":{"value":"=1+2","result":"3"}}HTTP/1.1 422 Unprocessable Entity
         |Connection: keep-alive
         |Content-Type: application/json; charset=utf-8
         |Content-Length: 29
         |
         |{"value":"","result":"ERROR"}""".stripMargin.replace("\r\n", "\n").replace("\n", "\r\n")
    val response = new String(socket.getInputStream.readNBytes(expectedResponse.getBytes(UTF_8).length), UTF_8)
    response shouldEqual expectedResponse
  }

}
