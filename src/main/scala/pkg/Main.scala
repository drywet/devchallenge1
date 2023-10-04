package pkg

import org.slf4j.{Logger, LoggerFactory}

object Main {

  val port: Int        = 8080
  val workerCount: Int = Runtime.getRuntime.availableProcessors

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val app = new AppHttpServerLauncher(port, workerCount, "volume/db", false)
    app.getCompleteFuture.handle { (_, e) =>
      logger.warn("Shutting down")
      app.close()
    }
    app.launch(Array.empty)
  }

}
