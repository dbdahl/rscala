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

  private val maxNArgs = 50
  private val argsLists = Array.range(1,maxNArgs).scanLeft("")((sum,i) => sum + ",x" + i).map(x => if ( x != "" ) x.substring(1) else x).map("(" + _ + ")")
  private val argsNames = Array.range(1,maxNArgs).scanLeft(List[String]())((sum,i) => ("x"+i) :: sum).map(_.reverse)

  private var size = 0
  private var valuesStack: List[Datum] = Nil
  private var namesStack: List[String] = Nil

  def pop[T](): T = {
    val x = valuesStack.head
    valuesStack = valuesStack.tail
    x.value.asInstanceOf[T]
  }

  private[server] def pushValue(x: Datum): Unit = {
    size += 1
    valuesStack = x :: valuesStack
  }

  private[server] def pushName(x: String): Unit = {
    namesStack = x :: namesStack
  }

  private[server] def reset(): Unit = {
    size = 0
    valuesStack = Nil
    namesStack = Nil
  }

  private[server] def argsList: String = argsLists(size)

  override def toString(): String = {
    val sb = new java.lang.StringBuilder()
    if ( namesStack == Nil ) namesStack = argsNames(size)
    valuesStack.zip(namesStack).foreach { x =>
      sb.append("val ")
      sb.append(x._2)
      sb.append(" = ES.pop[")
      sb.append(Protocol.typeMapper(x._1.tipe))
      sb.append("]()\n")
    }
    sb.toString
  }

}

object Server extends App {

  val debug = false
  val technique: Either[(Int,Int),(String,String)] = Left(9998,9999)
  //val technique: Either[(Int,Int),(String,String)] = Right("/home/dahl/docs/devel/rscala2/R/rscala2/pipe-s2r","/home/dahl/docs/devel/rscala2/R/rscala2/pipe-r2s")
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
      if ( Logger.enabled ) Logger("socket S2R waiting for client on port: "+portS2R)
      val sOut = serverOut.accept()
      if ( Logger.enabled ) Logger("socket R2S waiting for client on port: "+portR2S)
      val sIn = serverIn.accept()
      (sOut.getOutputStream, sIn.getInputStream)
    } else {
      val (pipeS2R, pipeR2S) = technique.right.get
      if ( Logger.enabled ) Logger("pipe S2R client is: "+pipeS2R)
      if ( Logger.enabled ) Logger("pipe R2S client is: "+pipeR2S)
      val fos = new FileOutputStream(pipeS2R)
      val fis = new FileInputStream(pipeR2S)
      (fos, fis)
    }
    val bos = if ( buffer ) new BufferedOutputStream(os) else os
    (new DataOutputStream(bos), new DataInputStream(is))
  }
  if ( Logger.enabled ) Logger("connections established")

  private val embeddedStack = new EmbeddedStack()    // called ES in the REPL
  private val functionCache = new HashMap[Int, (Any,String)]()

  def exit(): Unit = {
    if ( Logger.enabled ) Logger("exit")
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
    if ( Logger.enabled ) Logger("push")
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
    embeddedStack.pushValue(Datum(value,tipe))
  }

  def report(datum: Datum): Unit = {
    if ( Logger.enabled ) Logger("report")
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

  def invoke(): Unit = {
    if ( Logger.enabled ) Logger("invoke")
    val functionID = in.readInt()
    val (jvmFunction, resultType) = functionCache.getOrElse(functionID, {
      // throw new IllegalStateException("This function should be compiled first.")
      // This is where we compile the body
      (null, "Null")
    })
    /*
    // Debugging
    println("<---")
    (0 until nArgs).foreach { i => println(embeddedStack.pop[Any]()) }
    println("----")
    println(body)
    println("--->")
    */
    embeddedStack.reset()
    report(Datum(0,TCODE_INT_0))
  }

  def echo(): Unit = {
    val value = in.readInt()
    report(Datum(value,TCODE_INT_0))
  }

  @tailrec
  def loop(): Unit = {
    if ( Logger.enabled ) Logger("main")
    val request = in.readByte()
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH => push()
      case PCODE_INVOKE => invoke()
      case PCODE_ECHO => echo()
      case _ =>
        throw new IllegalStateException("Unsupported command: "+request)
    }
    loop()
  }

  loop()

}

