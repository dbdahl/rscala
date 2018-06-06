package org.ddahl.rscala2.server

import java.net._
import java.io._
import java.nio.ByteBuffer

import scala.annotation.tailrec


case class Datum(value: Any, tipe: Int)

object Server extends App {

  import Protocol._
  val serverOut = new ServerSocket(9998)
  val serverIn = new ServerSocket(9999)
  val sOut = serverOut.accept()
  val sIn = serverIn.accept()
  val out = new DataOutputStream(sOut.getOutputStream())
  val in = new DataInputStream(sIn.getInputStream())
  var stack = List[Datum]()

  def exit(): Unit = {
    out.close()
    in.close()
  }

  def push(): Unit = {
    val tipe = in.readInt()
    tipe match {
      case TCODE_INT_0 =>
        stack = Datum(in.readInt(), tipe) :: stack
      case TCODE_INT_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_INT)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        val value = Array.fill[Int](len) { byteBuffer.getInt() }
        stack = Datum(value, tipe) :: stack
      case TCODE_DOUBLE_0 =>
        stack = Datum(in.readInt(), tipe) :: stack
      case TCODE_DOUBLE_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_DOUBLE)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        val value = Array.fill[Double](len) { byteBuffer.getDouble() }
        stack = Datum(value, tipe) :: stack
      case _ =>
        println("Unsupported type.")
    }
  }

  def pop(): Unit = {
    val tipe = stack.head.tipe
    tipe match {
      case TCODE_INT_0 =>
        out.writeInt(tipe)
        out.writeInt(stack.head.value.asInstanceOf[Int])
      case TCODE_INT_1 =>
        val datum = stack.head
        val value = datum.value.asInstanceOf[Array[Int]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_INT)
        value.foreach(byteBuffer.putInt(_))
        out.writeInt(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_DOUBLE_0 =>
        out.writeInt(tipe)
        out.writeDouble(stack.head.value.asInstanceOf[Double])
      case TCODE_DOUBLE_1 =>
        val datum = stack.head
        val value = datum.value.asInstanceOf[Array[Double]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_DOUBLE)
        value.foreach(byteBuffer.putDouble(_))
        out.writeInt(tipe)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case _ =>
        println("Unsupported type.")
    }
    stack = stack.tail
  }

  @tailrec
  def loop(): Unit = {
    val request = in.readInt()
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH => push()
      case PCODE_POP  => pop()
      case _ =>
        println("WARNING: Unsupported request.")
    }
    loop()
  }

  loop()

}

