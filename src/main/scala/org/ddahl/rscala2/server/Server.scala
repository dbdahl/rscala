package org.ddahl.rscala2.server

import java.net._
import java.io._
import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

case class Datum(value: Any, tipe: Byte)

object Logger {

  var enabled = false

  def apply(x: String): Unit = if ( enabled ) println("DEBUG: "+x)

}

class EmbeddedStack {

  private var stack: List[Any] = Nil

  def pop[T](): T = {
    val x = stack.head
    stack = stack.tail
    x.asInstanceOf[T]
  }

  private[server] def set(x: List[Datum]): Unit = {
    stack = x.map(_.value)
  }

}

object Server extends App {

  val debug = true
  // val technique: Either[(Int,Int),(String,String)] = Left(9998,9999)
  val technique: Either[(Int,Int),(String,String)] = Right("/home/dahl/docs/devel/rscala2/pipe-s2r","/home/dahl/docs/devel/rscala2/pipe-r2s")
  val buffer = false

  Logger.enabled = debug

  import Protocol._

  Logger("starting server")

  private val (out, in) = {
    val buffer = false
    val (os,is) = if ( technique.isLeft ) {
      val (portS2R, portR2S) = technique.left.get
      val serverOut = new ServerSocket(portS2R)
      val serverIn = new ServerSocket(portR2S)
      Logger("socket S2R waiting for client on port: "+portS2R)
      val sOut = serverOut.accept()
      Logger("socket R2S waiting for client on port: "+portR2S)
      val sIn = serverIn.accept()
      (sOut.getOutputStream, sIn.getInputStream)
    } else {
      val (pipeS2R, pipeR2S) = technique.right.get
      Logger("pipe S2R client is: "+pipeS2R)
      Logger("pipe R2S client is: "+pipeR2S)
      (new FileOutputStream(pipeS2R), new FileInputStream(pipeR2S))
    }
    val bos = if ( buffer ) new BufferedOutputStream(os) else os
    (new DataOutputStream(bos), new DataInputStream(is))
  }
  Logger("connections established")

  private var stack = List[Datum]()
  private val embeddedStack = new EmbeddedStack()    // called ES in the REPL
  private val functionCache = new HashMap[String, (Any,String)]()

  def exit(): Unit = {
    Logger("exit")
    out.close()
    in.close()
  }

  private def readString(): String = {
    val nBytes = in.readInt()
    val bytes = new Array[Byte](nBytes)
    in.readFully(bytes)
    new String(bytes,"UTF-8")
  }

  def push(): Unit = {
    Logger("push")
    val tipe = in.readByte()
    val value = tipe match {
      case TCODE_INT_0 =>
        in.readInt()
      case TCODE_INT_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_INT)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill[Int](len) { byteBuffer.getInt() }
      case TCODE_DOUBLE_0 =>
        in.readDouble()
      case TCODE_DOUBLE_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_DOUBLE)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill[Double](len) { byteBuffer.getDouble() }
      case TCODE_CHARACTER_0 =>
        readString()
      case _ =>
        throw new IllegalStateException("Unsupported type.")
    }
    stack = Datum(value,tipe) :: stack
  }

  def report(datum: Datum): Unit = {
    Logger("report")
    val tipe = datum.tipe
    tipe match {
      case TCODE_INT_0 =>
        out.writeByte(tipe)
        out.writeInt(datum.value.asInstanceOf[Int])
      case TCODE_INT_1 =>
        val value = datum.value.asInstanceOf[Array[Int]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_INT)
        value.foreach(byteBuffer.putInt(_))
        out.writeByte(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_DOUBLE_0 =>
        out.writeByte(tipe)
        out.writeDouble(datum.value.asInstanceOf[Double])
      case TCODE_DOUBLE_1 =>
        val value = datum.value.asInstanceOf[Array[Double]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_DOUBLE)
        value.foreach(byteBuffer.putDouble(_))
        out.writeByte(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_CHARACTER_0 =>
        out.writeByte(tipe)
        val value = datum.value.asInstanceOf[String]
        val bytes = value.getBytes("UTF-8")
        out.writeInt(bytes.length)
        out.write(bytes)
      case _ =>
        throw new IllegalStateException("Unsupported type.")
    }
    out.flush()
  }

  def invokeWithNames(): Unit = {
    Logger("invoke with names")
    val nArgs = in.readInt()
    val names = List.fill(nArgs)(readString())
    val snippet = readString()
    val data = stack.take(nArgs).reverse
    embeddedStack.set(data)
    val sb = new java.lang.StringBuilder()
    sb.append("() => {\n")
    names.zip(data).foreach { triple =>
      sb.append("val ")
      sb.append(triple._1)
      sb.append(" = ES.pop[")
      sb.append(typeMapper(triple._2.tipe))
      sb.append("]()\n")
    }
    sb.append(snippet)
    sb.append("\n}")
    val body = sb.toString
    val (jvmFunction, resultType) = functionCache.getOrElse(body, {
      // This is where we compile the body
      (null, "Null")
    })
    report(Datum(0,TCODE_INT_0))
    // Debugging
//    println("<---")
//    (0 until nArgs).foreach { i => println(embeddedStack.pop[Any]()) }
//    println("----")
//    println(body)
//    println("--->")
  }

  def invokeWithoutNames(): Unit = {
    Logger("invoke without names")
    val nArgs = in.readInt()
    val snippet = readString()
    val data = stack.take(nArgs).reverse
    embeddedStack.set(data)
    val sb = new java.lang.StringBuilder()
    sb.append("() => {\n")
    data.zipWithIndex.foreach { triple =>
      sb.append("val x")
      sb.append(triple._2)
      sb.append(" = ES.pop[")
      sb.append(typeMapper(triple._1.tipe))
      sb.append("]()\n")
    }
    sb.append(snippet)
    sb.append("(")
    data.indices.foreach( i => {
      sb.append("x")
      sb.append(i)
      sb.append(",")
    })
    if ( ! data.isEmpty ) sb.deleteCharAt(sb.length-1)
    sb.append(")")
    sb.append("\n}")
    val body = sb.toString
    val (jvmFunction, resultType) = functionCache.getOrElse(body, {
      // This is where we compile the body
      (null, "Null")
    })
    report(Datum(0,TCODE_INT_0))
    // Debugging
//    println("<---")
//    (0 until nArgs).foreach { i => println(embeddedStack.pop[Any]()) }
//    println("----")
//    println(body)
//    println("--->")
  }

  @tailrec
  def loop(): Unit = {
    Logger("main")
    val request = in.readByte()
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH => push()
      case PCODE_INVOKE_WITH_NAMES => invokeWithNames()
      case PCODE_INVOKE_WITHOUT_NAMES => invokeWithoutNames()
      case _ =>
        throw new IllegalStateException("Unsupported command: "+request)
    }
    loop()
  }

  loop()

}

