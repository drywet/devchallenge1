package pkg

import io.activej.bytebuf.ByteBufStrings.encodeAscii
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ScheduledThreadPoolExecutor
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import scala.jdk.FutureConverters.CompletionStageOps
import scala.util.Using
import scala.util.matching.Regex

class TestHttpServerLauncherSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfterAll {

  val workerRegex: Regex = """#\d""".r

  val port                               = 8081
  val serverWorkerCount                  = 4
  val serverRequestDelay: FiniteDuration = 100.millis
  val socketCount: Int                   = serverWorkerCount * 10
  val requestCountPerSocket              = 10

  implicit val executionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(new ScheduledThreadPoolExecutor(socketCount + 10))

  val launcher = new TestHttpServerLauncher(port, serverWorkerCount, serverRequestDelay.toMillis)

  override protected def beforeAll(): Unit = {
    Future(launcher.launch(Array.empty))
    Await.ready(launcher.getStartFuture.asScala, 10.seconds)
  }

  override protected def afterAll(): Unit = {
    launcher.close()
    executionContext.shutdown()
  }

  /** Observation: all requests from all sockets are received by the server immediately without waiting 
   * for available worker threads.
   * Observation: if sending a lot of requests per connection at once, responses must be read as well, 
   * otherwise buffers would fill up and processing would stop */
  "TestHttpServerLauncher" should "process pipelined requests in order, by the same server worker" in {
    Using.Manager { use =>
      val sockets   = (0 until socketCount).map(_ => use(new Socket()))
      val localhost = new InetSocketAddress("localhost", port)
      sockets.foreach(_.connect(localhost))
      val results = sockets.zipWithIndex.map { case (socket, index) =>
        Future(
          pipeliningTest(
            socket = socket,
            socketId = index,
            requestCount = requestCountPerSocket,
            workerCount = serverWorkerCount
          )
        )
      }
      val expectedTime =
        serverRequestDelay * (socketCount / serverWorkerCount + 1) * requestCountPerSocket.toDouble + 10.seconds
      Await.result(Future.sequence(results), expectedTime)
    }.get
  }

  private def pipeliningTest(socket: Socket, socketId: Int, requestCount: Int, workerCount: Int) = {
    // println(s"${LocalDateTime.now}: writing $requestCount requests to socket $socketId")
    socket.getOutputStream.write(
      encodeAscii("""GET /abc HTTP/1.1
                    |Host: localhost
                    |Connection: keep-alive
                    |
                    |""".stripMargin.replace("\r\n", "\n").replace("\n", "\r\n") * requestCount)
    )
    // println(s"${LocalDateTime.now}: wrote $requestCount requests to socket $socketId")
    // println(s"${LocalDateTime.now}: reading all responses")
    val expectedResponseTemplate =
      s"""HTTP/1.1 200 OK
         |Connection: keep-alive
         |Content-Type: text/plain; charset=utf-8
         |Content-Length: 16
         |
         |Hello, world! #0""".stripMargin.replace("\r\n", "\n").replace("\n", "\r\n")
    val responses =
      new String(
        socket.getInputStream.readNBytes(expectedResponseTemplate.getBytes(UTF_8).length * requestCount),
        UTF_8
      )
    // println(s"${LocalDateTime.now}: index: $socketId, responses: $responses")
    val actualWorkerId = workerRegex.findAllMatchIn(responses).map(_.matched).reduce { (a, b) =>
      a shouldEqual b
      a
    }
    val expectedResponse = expectedResponseTemplate.replace("#0", actualWorkerId)
    // println(s"${LocalDateTime.now}: socket $socketId done, worker: $actualWorkerId")
    responses shouldEqual (expectedResponse * requestCount)
  }

}
