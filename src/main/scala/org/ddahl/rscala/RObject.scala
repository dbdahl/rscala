package org.ddahl.rscala

sealed trait RReference {

  val name: String

  override def toString = name

}

case class REphemeralReference(name: String) extends RReference
case class RPersistentReference(name: String) extends RReference

