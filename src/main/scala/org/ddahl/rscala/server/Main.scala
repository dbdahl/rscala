package org.ddahl.rscala.server

object Main extends App {

  // Once Java 6 support is dropped, we can replace polling with java.nio.file.WatchService
  object killer extends Thread {
    override def run() {
      val snippetFilename = new java.io.File(args(0))
      // Check every 20 seconds.
      while ( snippetFilename.exists ) Thread.sleep(20*1000)
      sys.exit(0)
    }
  }
  killer.setDaemon(true)
  killer.start()

  ScalaServer(args(0), args(1), args(2) == "TRUE", args(3) == "TRUE", args(4) == "TRUE", args(5).toInt, args(6) == "TRUE").run()

}

