package pkg

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, scanJsonValuesFromStream, writeToString}
import org.scalatest.flatspec._
import org.scalatest.matchers._
import pkg.Model._
import pkg.Timing.time

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8

class ModelSpec extends AnyFlatSpec with should.Matchers {

  private var iteration = 0

  it should "measure json performance" in {
    // scanJsonValuesFromStream is 10 times slower than readFromString
    (1 to 3).foreach { _ =>
      time("jsoniter perf", 1e5.toInt)(jsoniter())
      time("jsoniter streaming perf", 1e5.toInt)(jsoniterStreaming())
    }
  }

  def jsoniter(): Unit = {
    iteration += 1
    val request      = readFromString[PostCellRequest](s"""{"value":"Some text $iteration"}""")
    val responseJson = writeToString(PostCellResponse(request.value, request.value))
    require(responseJson == s"""{"value":"Some text $iteration","result":"Some text $iteration"}""")
  }

  def jsoniterStreaming(): Unit = {
    iteration += 1
    val requestStr = s"""{"value":"Some text $iteration"}\n{"value":"Some text ${-iteration}"}""".getBytes(UTF_8)
    val requests   = new Array[PostCellRequest](2)
    var i          = 0
    scanJsonValuesFromStream[PostCellRequest](new ByteArrayInputStream(requestStr)) { request =>
      requests(i) = request
      i += 1
      i < 2
    }
    val responseJson =
      requests.map(request => writeToString(PostCellResponse(request.value, request.value))).mkString("\n")
    require(
      responseJson == s"""{"value":"Some text $iteration","result":"Some text $iteration"}\n{"value":"Some text ${-iteration}","result":"Some text ${-iteration}"}"""
    )
  }

}
