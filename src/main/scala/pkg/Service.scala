package pkg

import java.util.concurrent.ConcurrentHashMap

class Service {
  val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()
}
