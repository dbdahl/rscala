package org.ddahl.rscala.server

import java.io.{File, PrintWriter, DataOutputStream, DataInputStream, BufferedOutputStream}
import java.net.{InetAddress, ServerSocket}

class Sockets(val port: Int, val buffer: Boolean, val debugger: Debugger) {

   // Start server
  if ( debugger.on ) debugger("starting server.")
  private val ( portS2R, portR2S) = if ( port == 0 ) (0, 0) else (port, port+1)
  private val localhost = InetAddress.getByName(null)
  private val serverOut = new ServerSocket(portS2R,0, localhost)
  private val serverIn = new ServerSocket(portR2S,0, localhost)

  val outPort = serverOut.getLocalPort
  val inPort = serverIn.getLocalPort

  def pid: String = {  // Starting with Java 9, ProcessHandle.current.pid is the cross-platform and simple solution.
    def ensureLong(x: String, fallback: String = "-1"): String = {
      try {
        x.toLong.toString
      } catch {
        case _: Throwable => fallback
      }
    }
    import sys.process._
    val selfFile = new File("/proc/self")
    // Note that Scala 2.11 doesn't have util.Properties.isLinux
    if ( selfFile.exists && !util.Properties.isWin && !util.Properties.isMac ) ensureLong(selfFile.getCanonicalFile.getName)
    else {
      val jvmName = java.lang.management.ManagementFactory.getRuntimeMXBean.getName
      val tmp = jvmName.indexOf("@") match {
        case -1 => "-1"
        case i => ensureLong(jvmName.take(i))
      }
      if ( tmp == "-1" && util.Properties.isMac ) ensureLong(Seq("sh", "-c", "echo $PPID").!!.trim)
      else tmp
    }
  }

  def writePortsFile(portsFilename: String, printWriter: PrintWriter) = {
    try {
      val portsFile = new File(portsFilename)
      val p = new PrintWriter(portsFile)
      p.println("" + outPort + " " + inPort + " " + pid)
      p.close()
    } catch {
      case e: Throwable => // R has already exited?
        if (debugger.on) {
          printWriter.println(e)
          e.printStackTrace(printWriter)
          debugger("cannot write to ports file, exiting...")
        }
        sys.exit(0)
    }
  }

  def acceptAndSetup(): (DataOutputStream, DataInputStream) = {
    if (debugger.on) debugger("socket S2R waiting for client on port " + serverOut.getLocalPort + ".")
    val sOut = serverOut.accept()
    if (debugger.on) debugger("socket R2S waiting for client on port " + serverIn.getLocalPort + ".")
    val sIn = serverIn.accept()
    val bos = if ( buffer ) new BufferedOutputStream(sOut.getOutputStream) else sOut.getOutputStream
    val out = new DataOutputStream(bos)
    val in = new DataInputStream(sIn.getInputStream)
    if ( debugger.on ) debugger("connections established.")
    (out,in)
  }

}

