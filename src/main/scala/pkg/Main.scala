package pkg

object Main {

  val port: Int        = 8080
  val workerCount: Int = Runtime.getRuntime.availableProcessors

  def main(args: Array[String]): Unit =
    new AppHttpServerLauncher(port, workerCount).launch(Array.empty)

}
