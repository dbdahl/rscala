package org.ddahl.rscala

import Protocol._

import scala.collection.mutable.HashMap

class Conduit(referenceMap: HashMap[Int, (Any,String)], debugger: Debugger) {

  private val maxNArgs: Int = 50
  private val argsNames = Array.range(1,maxNArgs+1).map("x"+_)
  private val argsLists = Array.range(1,maxNArgs).scanLeft("")((sum,i) => sum + ",x" + i).map(x => if ( x != "" ) x.substring(1) else x).map("(" + _ + ")")
  argsLists(0) = ""

  private var stack: List[(Datum,Option[String])] = Nil

  def pop[T](): T = {
    val x = stack.head
    stack = stack.tail
    x._1.value.asInstanceOf[T]
  }

  private[rscala] def push(x: Datum, name: Option[String]): Unit = {
    stack = (x,name) :: stack
  }

  private[rscala] def reset(nArgs: Int): Unit = {
    stack = stack.drop(nArgs)
  }

  def size: Int = stack.size

  private[rscala] def argsList(nArgs: Int, withReference: Boolean): String = argsLists(nArgs - ( if (withReference) 1 else 0))

  def mkHeader(nArgs: Int): String = {
    val sb = new java.lang.StringBuilder()
    stack.take(nArgs).zipWithIndex.foreach { x =>
      sb.append("val ")
      sb.append(x._1._2.getOrElse(argsNames(x._2)))
      sb.append(" = conduit.pop[")
      val tipe = x._1._1.tipe
      val tipeString = tipe match {
        case TCODE_REFERENCE =>
          x._1._1.msg.get
        case _ =>
          Protocol.typeMapper(tipe)
      }
      sb.append(tipeString)
      sb.append("]()\n")
    }
    sb.toString
  }

  var showCode = false

  def debug: Boolean = debugger.on

  def debug_=(value: Boolean) = {
    debugger.on = value
  }

}
