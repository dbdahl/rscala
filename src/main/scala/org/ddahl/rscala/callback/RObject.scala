package org.ddahl.rscala.callback

/** A class used to wrap an arbitrary R object.
*
* {{{
* val R = org.ddahl.rscala.callback.RClient()
* R.a = Array(1,2,3,4,5)
* val ref = R.evalR("as.list(a)")
* R.evalD0(s"sum(unlist(\${ref}))")
* }}}
*/
case class RObject(name: String) {

  override def toString() = ".$"+name

}

