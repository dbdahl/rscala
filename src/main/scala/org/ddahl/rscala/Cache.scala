package org.ddahl.rscala

class Cache {

  private val map = new scala.collection.mutable.ArrayBuffer[(Any,String)]()
  private val extractor = """\.(\d+)""".r

  def store(x: (Any, String)): String = {
    val result = """.""" + map.length
    map.append(x)
    result
  }

  def unapply(identifier: String): Option[(Any,String)] = identifier match {
    case extractor(i) => Some(map(i.toInt))
    case _ => None
  }

  def apply(identifier: String): (Any,String) = identifier match {
    case extractor(i) => map(i.toInt)
    case _ => map(identifier.toInt)
  }

  def apply(index: Int): (Any,String) = {
    map(index)
  }

  def free(identifier: String): Unit = identifier match {
    case extractor(i) => map(i.toInt) = null
    case _ => map(identifier.toInt) = null
  }

  def free(index: Int): Unit = {
    map(index) = null
  }

  def clear() = map.clear()

}

