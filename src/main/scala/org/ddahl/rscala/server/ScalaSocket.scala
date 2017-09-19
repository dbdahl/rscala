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
    if ( nBytes < buffer.remaining ) {
      flush()
      if ( nBytes > buffer.limit ) {
        _buffer = ByteBuffer.allocateDirect(nBytes)
      }
    }
  }

  // Helpers

  private def int2boolean(x: Int): Boolean = { x != 0 }
  private def boolean2int(x: Boolean): Int = { if (x) 1 else 0 }

  // Primitives

  def putInt(x: Int) = buffer.putInt(x)

  def getInt(): Int = buffer.getInt()

  def putDouble(x: Double) = buffer.putDouble(x)

  def getDouble(): Double = buffer.getDouble()

  def putBoolean(x: Boolean) = buffer.putInt(boolean2int(x))

  def getBoolean(): Boolean = int2boolean(buffer.getInt())

  def putByte(x: Byte) = buffer.put(x)

  def getByte(): Byte = buffer.get()

  // Tuples

  def getTuple2Int(): (Int,Int) = {
    inFill(2*bytesPerInt)
    (getInt(),getInt())
  }

  def getTuple3Int(): (Int,Int) = {
    inFill(3*bytesPerInt)
    (getInt(),getInt(),getInt())
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
    buffer.put(bytes)
  }

  def getScalarString(): String = {
    inFill(bytesPerInt)
    val nBytes = getInt()*bytesPerInt
    inFill(nBytes)
    val array = new Array[Byte](nBytes)
    buffer.get(array)
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
    buffer.asIntBuffer.get(array)
    array
  }

  def putVectorDouble(x: Array[Double]): Unit = {
    outFill(x.length*bytesPerDouble)
    for ( i <- 0 until x.length ) putDouble(x(i))
  }

  def getVectorDouble(length: Int): Array[Double] = {
    inFill(length*bytesPerDouble)
    val array = new Array[Double](length)
    buffer.asDoubleBuffer.get(array)
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
    sc.write(buffer2)
  }

  def getVectorByte(length: Int): Array[Byte] = {
    val buffer2 = ByteBuffer.allocate(length)
    sc.read(buffer2)
    buffer2.array
  }

  // Matrices

  def putMatrixInt(x: ArrayInt, ncol: Int, rowMajor: Boolean): Array[Array[Int]] = {
    val nrow =
    inFill(nrow*ncol*bytesPerInt)
    val buffer2 = buffer.asIntBuffer()
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

  def getMatrixInt(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Int]] = {
    inFill(nrow*ncol*bytesPerInt)
    val buffer2 = buffer.asIntBuffer()
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

  def getMatrixDouble(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Double]] = {
    inFill(nrow*ncol*bytesPerDouble)
    val buffer2 = buffer.asDoubleBuffer()
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

  def getMatrixBoolean(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Boolean]] = {
    inFill(nrow*ncol*bytesPerInt)
    val buffer2 = buffer.asIntBuffer()
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

  def getMatrixByte(nrow: Int, ncol: Int, rowMajor: Boolean): Array[Array[Byte]] = {
    val array = Array.fill(ncol) {
      val buffer2 = ByteBuffer.allocate(nrow)
      sc.read(buffer2)
      buffer2.array
    }
    if ( rowMajor ) {
      Array.tabulate(nrow)( i => Array.tabulate(ncol) ( j => array(j)(i) ) )
    } else array
  }

  def flush(): Unit = {
    buffer.flip()
    sc.write(buffer)
    buffer.clear()
  }

  def close() = sc.close()

}

