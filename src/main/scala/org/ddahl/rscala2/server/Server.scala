package org.ddahl.rscala2.server

import java.net._
import java.io._
import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

case class Datum(value: Any, tipe: Int)

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

  Logger.enabled = false

  import Protocol._
  val serverOut = new ServerSocket(9998)
  val serverIn = new ServerSocket(9999)
  val sOut = serverOut.accept()
  Logger("connected out")
  val sIn = serverIn.accept()
  Logger("connected in")
  val out = new DataOutputStream(sOut.getOutputStream())
  val in = new DataInputStream(sIn.getInputStream())

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
    val tipe = in.readInt()
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

  def pop(): Unit = {
    Logger("pop")
    val head = stack.head
    val tipe = head.tipe
    tipe match {
      case TCODE_INT_0 =>
        out.writeInt(tipe)
        out.writeInt(head.value.asInstanceOf[Int])
      case TCODE_INT_1 =>
        val value = head.value.asInstanceOf[Array[Int]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_INT)
        value.foreach(byteBuffer.putInt(_))
        out.writeInt(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_DOUBLE_0 =>
        out.writeInt(tipe)
        out.writeDouble(head.value.asInstanceOf[Double])
      case TCODE_DOUBLE_1 =>
        val value = head.value.asInstanceOf[Array[Double]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_DOUBLE)
        value.foreach(byteBuffer.putDouble(_))
        out.writeInt(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_CHARACTER_0 =>
        out.writeInt(tipe)
        val value = head.value.asInstanceOf[String]
        val bytes = value.getBytes("UTF-8")
        out.writeInt(bytes.length)
        out.write(bytes)
      case _ =>
        throw new IllegalStateException("Unsupported type.")
    }
    stack = stack.tail
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
    out.writeInt(RESULT_OKAY)
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
    out.writeInt(RESULT_OKAY)
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
    val request = in.readInt()
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH => push()
      case PCODE_POP  => pop()
      case PCODE_INVOKE_WITH_NAMES => invokeWithNames()
      case PCODE_INVOKE_WITHOUT_NAMES => invokeWithoutNames()
      case _ =>
        throw new IllegalStateException("Unsupported command.")
    }
    loop()
  }

  loop()

}

