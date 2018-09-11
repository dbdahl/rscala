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

  def writePortsFile(portsFilename: String, printWriter: PrintWriter) = {
    try {
      val portsFile = new File(portsFilename)
      val p = new PrintWriter(portsFile)
      p.println(outPort + " " + inPort)
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

