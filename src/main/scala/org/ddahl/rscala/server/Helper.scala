package org.ddahl.rscala.server

import java.io._

object Helper {

  def writeString(out: DataOutputStream, string: String): Unit = {
    val bytes = string.getBytes("UTF-8")
    val length = bytes.length
    out.writeInt(length)
    out.write(bytes,0,length)
  }

  def readString(in: DataInputStream): String = {
    val length = in.readInt()
    val bytes = new Array[Byte](length)
    in.readFully(bytes)
    new String(bytes,"UTF-8")
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

