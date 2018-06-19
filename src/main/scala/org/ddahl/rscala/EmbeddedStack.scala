package org.ddahl.rscala

import Protocol._

import scala.collection.mutable.HashMap

class EmbeddedStack(referenceMap: HashMap[Int, (Any,String)]) {

  private val maxNArgs: Int = 50
  private val argsLists = Array.range(1,maxNArgs).scanLeft("")((sum,i) => sum + ",x" + i).map(x => if ( x != "" ) x.substring(1) else x).map("(" + _ + ")")
  argsLists(0) = ""
  private val argsNames = Array.range(1,maxNArgs).scanLeft(List[String]())((sum,i) => ("x"+i) :: sum).map(_.reverse)

  private var _size: Int = 0
  private var valuesStack: List[Datum] = Nil
  private var namesStack: List[String] = Nil

  def pop[T](): T = {
    val x = valuesStack.head
    valuesStack = valuesStack.tail
    x.value.asInstanceOf[T]
  }

  private[rscala] def size: Int = _size

  private[rscala] def pushValue(x: Datum): Unit = {
    _size += 1
    valuesStack = x :: valuesStack
  }

  private[rscala] def pushName(x: String): Unit = {
    namesStack = x :: namesStack
  }

  private[rscala] def reset(): Unit = {
    _size = 0
    valuesStack = Nil
    namesStack = Nil
  }

  private[rscala] def argsList(withReference: Boolean): String = argsLists(_size - ( if (withReference) 1 else 0))

  override def toString(): String = {
    val sb = new java.lang.StringBuilder()
    if ( namesStack == Nil ) namesStack = argsNames(_size)
    valuesStack.zip(namesStack).foreach { x =>
      sb.append("val ")
      sb.append(x._2)
      sb.append(" = ES.pop[")
      val tipe = x._1.tipe
      val tipeString = tipe match {
        case TCODE_REFERENCE =>
          x._1.tipeString.get
        case _ =>
          Protocol.typeMapper(tipe)
      }
      sb.append(tipeString)
      sb.append("]()\n")
    }
    sb.toString
  }

}
