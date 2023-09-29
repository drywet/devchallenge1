package pkg

import java.util.concurrent.ConcurrentHashMap

class Service {
  // TODO sheet_id is case-insensitive
  val sheets: ConcurrentHashMap[String, Sheet] = new ConcurrentHashMap()
}
