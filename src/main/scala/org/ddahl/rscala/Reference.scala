package org.ddahl.rscala

/** A reference to an R object.
*
* @author David B. Dahl
*/
sealed trait Reference {

  val name: String

  override def toString = name

}

/** A reference to an R object which is only guaranteed to be valid in the scope in which it was created.  */
class EphemeralReference private[rscala] (val name: String, val scalaAsIs: Boolean) extends Reference

/** A reference to an R object which is only guaranteed to be valid in the scope in which it was created.  */
object EphemeralReference {

  def apply(name: String, scalaAsIs: Boolean = false) = new EphemeralReference(name, scalaAsIs)

}

/** A reference to an R object which persists beyond the scope in which it was created.  */
class PersistentReference private[rscala] (val name: String) extends Reference

/** A reference to an R object which persists beyond the scope in which it was created.  */
object PersistentReference {

  private[rscala] def apply(name: String) = new PersistentReference(name)

}

