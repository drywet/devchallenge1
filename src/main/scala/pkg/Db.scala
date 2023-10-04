package pkg

import org.rocksdb.{Options, RocksDB}

import java.nio.file.{Files, Paths}
import java.util.Comparator

/** Writes/reads are thread-safe, while objects like Iterator and WriteBatch require external synchronisation
 * https://github.com/facebook/rocksdb/wiki/Basic-Operations#concurrency */
class Db(dbPath: String, recreate: Boolean) extends AutoCloseable {

  if (recreate) {
    val path = Paths.get(dbPath)
    if (Files.isDirectory(path)) {
      Files
        .walk(path)
        .sorted(Comparator.reverseOrder())
        .map(_.toFile)
        .forEach(file => file.delete())
    } else if (Files.exists(path)) {
      path.toFile.delete()
    }
  }
  Files.createDirectories(Paths.get(dbPath))

  RocksDB.loadLibrary()
  val dbOptions: Options = new Options().setCreateIfMissing(true)
  val db: RocksDB        = RocksDB.open(dbOptions, dbPath)

  override def close(): Unit = {
    // This doesn't fsync WAL files, but they're synced on every write by default
    db.close()
    dbOptions.close()
  }

}
