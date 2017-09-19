package org.ddahl.rscala.server

import java.io.File
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress

import Protocol._

private[rscala] class ScalaSocket(portFilename: String, port: Int, initialBufferSize: Int, debugger: Debugger) {

  require(initialBufferSize >= 1024, "Buffer size should be at least 1024.")

  private val ssc = ServerSocketChannel.open()
  ssc.socket.bind(new InetSocketAddress(port))

  if ( debugger.value ) debugger.msg("Writing to port file: "+portFilename)
  locally {
    val portNumberFile = new File(portFilename)
    val p = new PrintWriter(portNumberFile)
    p.println(ssc.socket.getLocalPort)
    p.close()
  }
  if ( debugger.value ) debugger.msg("Server is running on port "+ssc.socket.getLocalPort)

  private val sc = ssc.accept()
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

  def outFill(nBytes: Int): Unit = {
    if ( buffer.remaining + nBytes.toLong > buffer.limit ) {
      flush()
      if ( nBytes > buffer.capacity ) {
        _buffer = ByteBuffer.allocateDirect(nBytes)
      }
    }
  }

  def putString(string: String): Unit = {
    val bytes = string.getBytes("UTF-8")
    outFill(bytesPerInt)
    buffer.putInt(bytes.length)
    outFill(bytes.length)
    buffer.put(bytes)
  }

  def getString(): String = {
    inFill(bytesPerInt)
    inFill(buffer.getInt()*bytesPerInt)
    val array = new Array[Byte](buffer.remaining)
    buffer.get(array)
    new String(array,"UTF-8")
  }

  def putInt(x: Int) = buffer.putInt(x)

  def getInt() = buffer.getInt()

  def read(x: ByteBuffer) = sc.read(x)

  def flush(): Unit = {
    buffer.flip()
    sc.write(buffer)
    buffer.clear()
  }

  def close() = sc.close()

}

