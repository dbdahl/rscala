package org.ddahl.rscala

/** A reference to an R object.
*
* @author David B. Dahl
*/
sealed trait RReference {

  val name: String

  override def toString = name

}

/** A reference to an R object which is only guaranteed to be valid in the scope in which it was created.  */
class REphemeralReference private[rscala] (val name: String) extends RReference

/** A reference to an R object which is only guaranteed to be valid in the scope in which it was created.  */
object REphemeralReference {

  def apply(name: String) = new REphemeralReference(name)

}

/** A reference to an R object which persists beyond the scope in which it was created.  */
class RPersistentReference private[rscala] (val name: String) extends RReference

/** A reference to an R object which persists beyond the scope in which it was created.  */
object RPersistentReference {

  private[rscala] def apply(name: String) = new RPersistentReference(name)

}

