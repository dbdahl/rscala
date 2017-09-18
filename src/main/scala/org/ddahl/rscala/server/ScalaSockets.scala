package org.ddahl.rscala.server

import java.io.File
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress

import Protocol._

private[rscala] class ScalaSockets(portFilename: String, port: Int, bufferSize: Int, debugger: Debugger) {

  require(bufferSize >= 1024, "Buffer size should be at least 1024.")

  val ssc = ServerSocketChannel.open()
  ssc.socket.bind(new InetSocketAddress(port))

  if ( debugger.value ) debugger.msg("Trying to open port filename: "+portFilename)
  locally {
    val portNumberFile = new File(portFilename)
    val p = new PrintWriter(portNumberFile)
    p.println(ssc.socket.getLocalPort)
    p.close()
  }
  if ( debugger.value ) debugger.msg("Server is running on port "+ssc.socket.getLocalPort)

  val sc = ssc.accept()
  sc.configureBlocking(true)
  sc.socket.setTcpNoDelay(true)

  val bytesPerInt = java.lang.Integer.BYTES
  val bytesPerDouble = java.lang.Double.BYTES
  val buffer = ByteBuffer.allocate(bufferSize*bytesPerInt)

}

