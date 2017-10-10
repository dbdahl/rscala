package org.ddahl.rscala.server

object Main extends App {

  // Once Java 6 support is dropped, we can replace polling with java.nio.file.WatchService
  object killer extends Thread {
    override def run() {
      val snippetFilename = new java.io.File(args(0))
      // Check every 10 seconds.
      val millis = 10*1000
      while ( snippetFilename.exists ) Thread.sleep(millis)
      sys.exit(1)
    }
  }
  killer.setDaemon(true)
  killer.start()

  ScalaServer(args(0), args(1), args(2) == "TRUE", args(3) == "TRUE", args(4) == "TRUE", args(5).toInt, args(6) == "TRUE").run()

}

