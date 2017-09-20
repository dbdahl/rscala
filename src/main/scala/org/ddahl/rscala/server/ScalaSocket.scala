package org.ddahl.rscala.server

import java.io.File
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress

private[rscala] class ScalaSocket(portFilename: String, port: Int, initialBufferSize: Int, debugger: Debugger) {

  require(initialBufferSize >= 1024, "Buffer size should be at least 1024.")

  private val sscIn = ServerSocketChannel.open()
  sscIn.socket.bind(new InetSocketAddress(port))

  private val sscOut = ServerSocketChannel.open()
  sscOut.socket.bind(new InetSocketAddress(if ( port == 0 ) 0 else port+1))

  if ( debugger.value ) debugger.msg("Writing to port file: "+portFilename)
  locally {
    val portNumberFile = new File(portFilename)
    val p = new PrintWriter(portNumberFile)
    p.println(sscIn.socket.getLocalPort+" "+sscOut.socket.getLocalPort)
    p.close()
  }
  if ( debugger.value ) debugger.msg("Server is running on ports " + sscIn.socket.getLocalPort +" and " + sscOut.socket.getLocalPort)

  private val scIn = sscIn.accept()
  scIn.configureBlocking(true)
  scIn.socket.setTcpNoDelay(true)

  private val scOut = sscOut.accept()
  scOut.configureBlocking(true)
  scOut.socket.setTcpNoDelay(true)

  val bytesPerInt = java.lang.Integer.BYTES
  val bytesPerDouble = java.lang.Double.BYTES

  private var _bufferIn  = ByteBuffer.allocateDirect(initialBufferSize)
  private var _bufferOut = ByteBuffer.allocateDirect(initialBufferSize)

  private def bufferIn  = _bufferIn
  private def bufferOut = _bufferOut

  private def inFill(nBytes: Int): Unit = {
    if ( debugger.value ) debugger.msg("Reading "+nBytes+" bytes.")
    if ( nBytes > bufferIn.capacity ) {
      _bufferIn = ByteBuffer.allocateDirect(nBytes)
    } else {
      bufferIn.clear()
      bufferIn.limit(nBytes)
    }
    while ( bufferIn.hasRemaining ) {
      if ( scIn.read(bufferIn) == -1 ) throw new IllegalStateException("Socket is unexpectedly closed.")
    }
    bufferIn.flip()
  }

  private def outFill(nBytes: Int): Unit = {
    if ( nBytes > bufferOut.remaining ) {
      flush()
      if ( nBytes > bufferOut.limit ) {
        _bufferOut = ByteBuffer.allocateDirect(nBytes)
      }
    }
  }

  // Helpers

  def int2boolean(x: Int): Boolean = { x != 0 }
  def boolean2int(x: Boolean): Int = { if (x) 1 else 0 }
  def double2boolean(x: Double): Boolean = { x != 0.0 }
  def boolean2double(x: Boolean): Double = { if (x) 1.0 else 0.0 }
  def string2boolean(x: String): Boolean = { x.toBoolean }
  def boolean2string(x: Boolean): String = { x.toString }
  def byte2boolean(x: Byte): Boolean = { x != 0.toByte }
  def boolean2byte(x: Boolean): Byte = { if (x) 1.toByte else 0.toByte }

  def rowsColumns[T](x: Array[Array[T]], rowMajor: Boolean): (Int,Int) = {
    if ( rowMajor ) ( x.length, if ( x.length == 0 ) 0 else x(0).length )
    else ( if ( x.length == 0 ) 0 else x(0).length, x.length )
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

  // Primitives

  private def putInt(x: Int) = bufferOut.putInt(x)

  private def getInt(): Int = bufferIn.getInt()

  private def putDouble(x: Double) = bufferOut.putDouble(x)

  private def getDouble(): Double = bufferIn.getDouble()

  private def putBoolean(x: Boolean) = bufferOut.putInt(boolean2int(x))

  private def getBoolean(): Boolean = int2boolean(bufferIn.getInt())

  private def putByte(x: Byte) = bufferOut.put(x)

  private def getByte(): Byte = bufferIn.get()

  // Tuples

  def putTuple2Int(x: Int, y: Int): Unit = {
    outFill(2*bytesPerInt)
    putInt(x)
    putInt(y)
  }

  def getTuple2Int(): (Int,Int) = {
    inFill(2*bytesPerInt)
    (getInt(),getInt())
  }

  def putTuple3Int(x: Int, y: Int, z: Int): Unit = {
    outFill(3*bytesPerInt)
    putInt(x)
    putInt(y)
    putInt(z)
  }

  def getTuple3Int(): (Int,Int,Int) = {
    inFill(3*bytesPerInt)
    (getInt(),getInt(),getInt())
  }

  def putTuple4Int(x: Int, y: Int, z: Int, a: Int): Unit = {
    outFill(4*bytesPerInt)
    putInt(x)
    putInt(y)
    putInt(z)
    putInt(a)
  }

  def getTuple4Int(): (Int,Int,Int,Int) = {
    inFill(4*bytesPerInt)
    (getInt(),getInt(),getInt(),getInt())
  }

  // Scalars

  def putScalarInt(x: Int) = {
    outFill(bytesPerInt)
    putInt(x)
  }

  def getScalarInt(): Int = {
    inFill(bytesPerInt)
    getInt()
  }

  def putScalarDouble(x: Double) = {
    outFill(bytesPerDouble)
    putDouble(x)
  }

  def getScalarDouble(): Double = {
    inFill(bytesPerDouble)
    getDouble()
  }

  def putScalarBoolean(x: Boolean) = {
    outFill(bytesPerInt)
    putInt(boolean2int(x))
  }

  def getScalarBoolean(): Boolean = {
    inFill(bytesPerInt)
    int2boolean(getInt())
  }

  def putScalarString(string: String): Unit = {
    val bytes = string.getBytes("UTF-8")
    outFill(bytesPerInt)
    putInt(bytes.length)
    outFill(bytes.length)
    bufferOut.put(bytes)
  }

  def getScalarString(): String = {
    inFill(bytesPerInt)
    val nBytes = getInt()
    inFill(nBytes)
    val array = new Array[Byte](nBytes)
    bufferIn.get(array)
    new String(array,"UTF-8")
  }

  def putScalarByte(x: Byte) = {
    outFill(1)
    putByte(x)
  }

  def getScalarByte(): Byte = {
    inFill(1)
    getByte()
  }

  // Vectors

  def putVectorInt(x: Array[Int]): Unit = {
    outFill(x.length*bytesPerInt)
    for ( i <- 0 until x.length ) putInt(x(i))
  }

  def getVectorInt(length: Int): Array[Int] = {
    inFill(length*bytesPerInt)
    val array = new Array[Int](length)
    bufferIn.asIntBuffer.get(array)
    array
  }

  def putVectorDouble(x: Array[Double]): Unit = {
    outFill(x.length*bytesPerDouble)
    for ( i <- 0 until x.length ) putDouble(x(i))
  }

  def getVectorDouble(length: Int): Array[Double] = {
    inFill(length*bytesPerDouble)
    val array = new Array[Double](length)
    bufferIn.asDoubleBuffer.get(array)
    array
  }

  def putVectorBoolean(x: Array[Boolean]): Unit = {
    outFill(x.length*bytesPerInt)
    for (i <- 0 until x.length) putBoolean(x(i))
  }

  def getVectorBoolean(length: Int): Array[Boolean] = {
    inFill(length*bytesPerInt)
    Array.fill(length) { getBoolean() }
  }

  def putVectorString(x: Array[String]): Unit = {
    for ( i <- 0 until x.length ) putScalarString(x(i))
  }

  def getVectorString(length: Int): Array[String] = {
    Array.fill(length) { getScalarString() }
  }

  def putVectorByte(x: Array[Byte]): Unit = {
    val buffer2 = ByteBuffer.wrap(x)
    flush()
    scOut.write(buffer2)
  }

  def getVectorByte(length: Int): Array[Byte] = {
    val buffer2 = ByteBuffer.allocate(length)
    while ( buffer2.hasRemaining ) {
      if ( scIn.read(buffer2) == -1 ) throw new IllegalStateException("Socket is unexpectedly closed.")
    }
    buffer2.array
  }

  // Matrices

  def putMatrixInt(x: Array[Array[Int]], rowMajor: Boolean): Unit = {
    val (nrow, ncol) = rowsColumns(x,rowMajor)
    outFill(nrow*ncol*bytesPerInt)
    for ( j <- 0 until ncol ) {
      for ( i <- 0 until nrow ) {
        putInt(if ( rowMajor ) x(i)(j) else x(j)(i))
      }
    }
  }

  def getMatrixInt(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Int]] = {
    inFill(nrow*ncol*bytesPerInt)
    val buffer2 = bufferIn.asIntBuffer()
    if ( rowMajor ) Array.tabulate(nrow) { i =>
      Array.tabulate(ncol) { j =>
        buffer2.get(j*nrow + i)
      }
    } else Array.fill(ncol) {
      val array = new Array[Int](nrow)
      buffer2.get(array)
      array
    }
  }

  def putMatrixDouble(x: Array[Array[Double]], rowMajor: Boolean): Unit = {
    val (nrow, ncol) = rowsColumns(x,rowMajor)
    outFill(nrow*ncol*bytesPerDouble)
    for ( j <- 0 until ncol ) {
      for ( i <- 0 until nrow ) {
        putDouble(if ( rowMajor ) x(i)(j) else x(j)(i))
      }
    }
  }

  def getMatrixDouble(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Double]] = {
    inFill(nrow*ncol*bytesPerDouble)
    val buffer2 = bufferIn.asDoubleBuffer()
    if ( rowMajor ) Array.tabulate(nrow) { i =>
      Array.tabulate(ncol) { j =>
        buffer2.get(j*nrow + i)
      }
    } else Array.fill(ncol) {
      val array = new Array[Double](nrow)
      buffer2.get(array)
      array
    }
  }

  def putMatrixBoolean(x: Array[Array[Boolean]], rowMajor: Boolean): Unit = {
    val (nrow, ncol) = rowsColumns(x,rowMajor)
    outFill(nrow*ncol*bytesPerInt)
    for ( j <- 0 until ncol ) {
      for ( i <- 0 until nrow ) {
        putBoolean(if ( rowMajor ) x(i)(j) else x(j)(i))
      }
    }
  }

  def getMatrixBoolean(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Boolean]] = {
    inFill(nrow*ncol*bytesPerInt)
    val buffer2 = bufferIn.asIntBuffer()
    if ( rowMajor ) Array.tabulate(nrow) { i =>
      Array.tabulate(ncol) { j =>
        int2boolean(buffer2.get(j*nrow + i))
      }
    } else Array.fill(ncol) {
      Array.fill(nrow) {
        int2boolean(buffer2.get())
      }
    }
  }

  def putMatrixString(x: Array[Array[String]], rowMajor: Boolean): Unit = {
    val (nrow, ncol) = rowsColumns(x,rowMajor)
    for ( j <- 0 until ncol ) {
      for ( i <- 0 until nrow ) {
        putScalarString(if ( rowMajor ) x(i)(j) else x(j)(i))
      }
    }
  }

  def getMatrixString(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[String]] = {
    val array = Array.fill(ncol) {
      Array.fill(nrow) {
        getScalarString()
      }
    }
    if ( rowMajor ) {
      Array.tabulate(nrow)( i => Array.tabulate(ncol) ( j => array(j)(i) ) )
    } else array
  }

  def putMatrixByte(x: Array[Array[Byte]], rowMajor: Boolean): Unit = {
    val (nrow, ncol) = rowsColumns(x,rowMajor)
    outFill(nrow*ncol)
    for ( j <- 0 until ncol ) {
      for ( i <- 0 until nrow ) {
        putByte(if ( rowMajor ) x(i)(j) else x(j)(i))
      }
    }
  }

  def getMatrixByte(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Byte]] = {
    val array = Array.fill(ncol) {
      val buffer2 = ByteBuffer.allocate(nrow)
      while ( buffer2.hasRemaining ) {
        if ( scIn.read(buffer2) == -1 ) throw new IllegalStateException("Socket is unexpectedly closed.")
      }
      buffer2.array
    }
    if ( rowMajor ) {
      Array.tabulate(nrow)( i => Array.tabulate(ncol) ( j => array(j)(i) ) )
    } else array
  }

  def flush(): Unit = {
    bufferOut.flip()
    while ( bufferOut.hasRemaining() ) scOut.write(bufferOut)
    bufferOut.clear()
  }

  def close() = {
    scIn.close()
    scOut.close()
    sscIn.close()
    sscOut.close()
  }

}

