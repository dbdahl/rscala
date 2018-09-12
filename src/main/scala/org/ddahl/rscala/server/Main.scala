package org.ddahl.rscala.server

import scala.collection.mutable.HashMap
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings
import java.io.{BufferedOutputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, File, PrintWriter}
import java.net.{InetAddress, ServerSocket}
import java.util.concurrent.TimeUnit
import java.nio.file.{FileSystems, Files, NoSuchFileException, Paths, StandardWatchEventKinds}

object Main extends App {

  val rscalaClasspath = args(0)
  val port = args(1).toInt
  val portsFilename = args(2)
  val sessionFilename = args(3)
  val debug = (args(4) == "TRUE")
  val serializeOutput = (args(5) == "TRUE")
  val buffer = (args(6) == "TRUE")

  // Set up sinks
  val (debugger, prntWrtr, baos) = serializeOutput match {
    case true =>
      val out = new ByteArrayOutputStream()
      val pw = new PrintWriter(out, false)
      val d = new Debugger(debug, pw, "Scala", false)
      (d, pw, out)
    case false =>
      val pw = new PrintWriter(System.out, true)
      val d = new Debugger(debug, pw, "Scala", false)
      (d, pw, null)
  }

  // Start session killer
  if ( debugger.on ) debugger("starting session killer.")

  object killer extends Thread {

    override def run(): Unit = {
      val watchService = FileSystems.getDefault().newWatchService()
      val sessionFile = Paths.get(sessionFilename)
      if ( ! Files.exists(sessionFile) ) {
        if ( debugger.on ) debugger("session file has already been deleted (#1), exiting...")
        sys.exit(0)
      }
      try {
        sessionFile.getParent.register(watchService, StandardWatchEventKinds.ENTRY_DELETE)
      } catch {
        case e: NoSuchFileException => {
          if ( debugger.on ) debugger("session file has already been deleted (#2), exiting...")
          sys.exit(0)
        }
      }
      while (true) {
        // Check for file existence every 10 seconds or whenever R's temp directory changes.
        val watchKey = watchService.poll(10, TimeUnit.SECONDS)
        if ( watchKey != null ) {
          watchKey.pollEvents()
          watchKey.reset()
        }
        if ( ! Files.exists(sessionFile) ) {
          if ( debugger.on ) debugger("session file no longer exists, exiting...")
          sys.exit(0)
        }
      }
    }

  }

  killer.setDaemon(true)
  killer.start()

  // Instantiate interpreter
  if ( debugger.on ) debugger("starting interpreter.")

  val settings = new Settings()
  settings.embeddedDefaults[Datum]
  settings.classpath.value = rscalaClasspath
  settings.deprecation.value = true
  settings.feature.value = true
  settings.unchecked.value = true
  settings.language.add("reflectiveCalls")

  val intp = ServerStub.mkIMain(settings, prntWrtr)

  // Set up interpreter
  val referenceMap = new HashMap[Int, (Any,String)]()

  if ( debugger.on ) debugger("binding conduit.")
  val conduit = new Conduit(debugger)
  intp.bind("conduit",conduit)

  if ( debugger.on ) debugger("binding r client.")
  val rClient = new org.ddahl.rscala.RClient()
  intp.bind("R",rClient)

  val sockets = new Sockets(port, buffer, debugger)
  sockets.writePortsFile(portsFilename,prntWrtr)
  val (out,in) = sockets.acceptAndSetup()

  // Start main loop
  if ( debugger.on ) debugger("entering main loop.")
  val server = new Server(intp, sockets, referenceMap, conduit, out, in, debugger, serializeOutput, prntWrtr, baos)
  rClient.server = server
  server.run()

}

