package org.ddahl.rscala.server

import java.nio.file._
import collection.JavaConverters._

object Main extends App {

  object killer extends Thread {
    override def run(): Unit = {
      val watchService = FileSystems.getDefault().newWatchService()
      val snippetFileFullPath = Paths.get(args(0))
      val snippetFile = snippetFileFullPath.getFileName
      try {
        snippetFileFullPath.getParent.register(watchService, StandardWatchEventKinds.ENTRY_DELETE)
      } catch {
        case e: NoSuchFileException => sys.exit(0)      // R already exited!
      }
      while (true) {
        val watchKey = watchService.take()
        for ( event <- watchKey.pollEvents().asScala ) {
          val path = event.context.asInstanceOf[Path]
          if ( path.equals(snippetFile) ) sys.exit(0)
        }
        watchKey.reset()
      }
    }
  }
  killer.setDaemon(true)
  killer.start()

  ScalaServer(args(0), args(1), args(2) == "TRUE", args(3) == "TRUE", args(4) == "TRUE", args(5).toInt, args(6) == "TRUE").run()

}

