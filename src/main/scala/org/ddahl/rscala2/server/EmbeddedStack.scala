package org.ddahl.rscala2.server

class EmbeddedStack {

  private val maxNArgs: Int = 50
  private val argsLists = Array.range(1,maxNArgs).scanLeft("")((sum,i) => sum + ",x" + i).map(x => if ( x != "" ) x.substring(1) else x).map("(" + _ + ")")
  private val argsNames = Array.range(1,maxNArgs).scanLeft(List[String]())((sum,i) => ("x"+i) :: sum).map(_.reverse)

  private var _size: Int = 0
  private var valuesStack: List[Datum] = Nil
  private var namesStack: List[String] = Nil

  def pop[T](): T = {
    val x = valuesStack.head
    valuesStack = valuesStack.tail
    x.value.asInstanceOf[T]
  }

  private[server] def size: Int = _size

  private[server] def pushValue(x: Datum): Unit = {
    _size += 1
    valuesStack = x :: valuesStack
  }

  private[server] def pushName(x: String): Unit = {
    namesStack = x :: namesStack
  }

  private[server] def reset(): Unit = {
    _size = 0
    valuesStack = Nil
    namesStack = Nil
  }

  private[server] def argsList: String = argsLists(_size)

  override def toString(): String = {
    val sb = new java.lang.StringBuilder()
    if ( namesStack == Nil ) namesStack = argsNames(_size)
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
