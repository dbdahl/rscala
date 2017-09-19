package org.ddahl.rscala.server

import java.io.File
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress

import Protocol._

private[rscala] class ScalaSockets(portFilename: String, port: Int, initialBufferSize: Int, debugger: Debugger) {

  require(initialBufferSize >= 1024, "Buffer size should be at least 1024.")

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

  private var _buffer = ByteBuffer.allocateDirect(initialBufferSize)

  def buffer = _buffer

  def inFill(nBytes: Int): Unit = {
    if ( nBytes > buffer.capacity ) {
      _buffer = ByteBuffer.allocateDirect(nBytes)
    } else {
      buffer.clear()
      buffer.limit(nBytes)
    }
    sc.read(buffer)
    buffer.flip()
  }

  def readString(): String = {
    inFill(bytesPerInt)
    inFill(buffer.getInt()*bytesPerInt)
    sc.read(buffer)
    val array = new Array[Byte](buffer.remaining)
    buffer.get(array)
    new String(array,"UTF-8")
  }

  def outFill(nBytes: Int): Unit = {
    if ( buffer.remaining + nBytes.toLong > buffer.limit ) {
      buffer.flip()
      sc.write(buffer)
      if ( nBytes > buffer.capacity ) {
        _buffer = ByteBuffer.allocateDirect(nBytes)
      } else {
        buffer.clear()
      }
    }
  }

  def writeString(string: String): Unit = {
    val bytes = string.getBytes("UTF-8")
    outFill(bytesPerInt)
    buffer.putInt(bytes.length)
    outFill(bytes.length)
    buffer.put(bytes)
  }

}

