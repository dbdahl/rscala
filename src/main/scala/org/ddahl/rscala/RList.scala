package org.ddahl.rscala

trait RList {

  val names: Array[String]
  val namesOriginal: Array[String]
  val isDataFrame: Boolean

  assert(names.length == namesOriginal.length)

}
