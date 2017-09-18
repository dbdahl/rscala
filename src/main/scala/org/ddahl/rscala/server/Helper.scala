package org.ddahl.rscala.server

import java.nio.channels.SocketChannel
import java.nio.ByteBuffer

object Helper {

  def writeString(buffer: ByteBuffer, string: String): Unit = {
    val bytes = string.getBytes("UTF-8")
    buffer.putInt(bytes.length)
    buffer.put(bytes)
  }

  def readString(sc: SocketChannel, buffer: ByteBuffer): String = {
    buffer.clear()
    buffer.limit(java.lang.Integer.BYTES)
    sc.read(buffer)
    val neededCapacity = buffer.getInt()*java.lang.Integer.BYTES
    if ( neededCapacity > buffer.capacity ) {
      throw new RuntimeException("Expanding string capacity is not currently supported.")
    }
    buffer.clear()
    buffer.limit(neededCapacity)
    sc.read(buffer)
    new String(buffer.array,0,neededCapacity,"UTF-8")
  }

  def isMatrix[T](x: Array[Array[T]]): Boolean = {
    if ( x.length != 0 ) {
      val len = x(0).length
      for ( i <- 1 until x.length ) {
        if ( x(i).length != len ) return false
      }
    }
    true
  }

  def transposeIfNot[D](x: Array[Array[D]], rowMajor: Boolean)(implicit tag: scala.reflect.ClassTag[D]): Array[Array[D]] = {
    if ( rowMajor ) x
    else {
      val r = x.length
      if ( r == 0 ) return(new Array[Array[D]](0))
      val c = x(0).length
      if ( c == 0 ) return(new Array[Array[D]](0))
      Array.tabulate(c)( j => Array.tabulate(r) ( i => x(i)(j) ) )
    }
  }

}

