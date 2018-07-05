package org.ddahl.rscala

import Protocol._

final class RObject private[rscala] (val x: Array[Byte]) {

  private def canEqual(a: Any): Boolean = a.isInstanceOf[RObject]

  override def equals(that: Any): Boolean = that match {
    case that: RObject => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = java.util.Arrays.hashCode(x)

}

class RClient() {

  private[rscala] var server: Server = null

  def eval  (template: String, values: Any*): Unit                  = evalWithoutResult                     (template, values)

  def evalI0(template: String, values: Any*): Int                   = evalWithResult[Int]                   (template, values, "storage.mode(.rs) <- 'integer'; .rs[1]")
  def evalI1(template: String, values: Any*): Array[Int]            = evalWithResult[Array[Int]]            (template, values, "storage.mode(.rs) <- 'integer'; I(.rs)")
  def evalI2(template: String, values: Any*): Array[Array[Int]]     = evalWithResult[Array[Array[Int]]]     (template, values, "storage.mode(.rs) <- 'integer'; .rs")

  def evalD0(template: String, values: Any*): Double                = evalWithResult[Double]                (template, values, "storage.mode(.rs) <- 'double'; .rs[1]")
  def evalD1(template: String, values: Any*): Array[Double]         = evalWithResult[Array[Double]]         (template, values, "storage.mode(.rs) <- 'double'; I(.rs)")
  def evalD2(template: String, values: Any*): Array[Array[Double]]  = evalWithResult[Array[Array[Double]]]  (template, values, "storage.mode(.rs) <- 'double'; .rs")

  def evalL0(template: String, values: Any*): Boolean               = evalWithResult[Boolean]               (template, values, "storage.mode(.rs) <- 'logical'; .rs[1]")
  def evalL1(template: String, values: Any*): Array[Boolean]        = evalWithResult[Array[Boolean]]        (template, values, "storage.mode(.rs) <- 'logical'; I(.rs)")
  def evalL2(template: String, values: Any*): Array[Array[Boolean]] = evalWithResult[Array[Array[Boolean]]] (template, values, "storage.mode(.rs) <- 'logical'; .rs")

  def evalR0(template: String, values: Any*): Byte                  = evalWithResult[Byte]                  (template, values, "storage.mode(.rs) <- 'raw'; .rs[1]")
  def evalR1(template: String, values: Any*): Array[Byte]           = evalWithResult[Array[Byte]]           (template, values, "storage.mode(.rs) <- 'raw'; I(.rs)")
  def evalR2(template: String, values: Any*): Array[Array[Byte]]    = evalWithResult[Array[Array[Byte]]]    (template, values, "storage.mode(.rs) <- 'raw'; .rs")

  def evalS0(template: String, values: Any*): String                = evalWithResult[String]                (template, values, "storage.mode(.rs) <- 'character'; .rs[1]")
  def evalS1(template: String, values: Any*): Array[String]         = evalWithResult[Array[String]]         (template, values, "storage.mode(.rs) <- 'character'; I(.rs)")
  def evalS2(template: String, values: Any*): Array[Array[String]]  = evalWithResult[Array[Array[String]]]  (template, values, "storage.mode(.rs) <- 'character'; .rs")

  def evalObject(template: String, values: Any*): RObject = {
    val template2 = "I(serialize({" + template + "},NULL))"
    evalEngine(template2, values)
    val x = server.conduit.pop[Array[Byte]]
    new RObject(x)
  }

  private def evalWithoutResult[A](template: String, values: Seq[Any]): Unit = {
    evalEngine(template + "; NULL", values)
    server.conduit.pop[Any]()
  }

  private def evalWithResult[A](template: String, values: Seq[Any], casting: String): A = {
    evalEngine(".rs <- {" + template + "}; " + casting, values)
    server.conduit.pop[A]
  }

  private def evalEngine(template: String, values: Seq[Any]): Unit = {
    server.pop(Datum(values.length, TCODE_CALLBACK, Some(template)))
    values.foreach(v => server.pop(any2Datum(v)))
    server.run()
    if ( server.getCmd() != PCODE_PUSH_WITHOUT_NAME ) throw new RuntimeException("Protocol error.")
    server.push(false)
  }

  private def any2Datum(any: Any): Datum = {
    val c = any.getClass
    val tipe = if (c.isArray) {
      c.getName match {
        case "[I" => TCODE_INT_1
        case "[D" => TCODE_DOUBLE_1
        case "[Z" => TCODE_LOGICAL_1
        case "[B" => TCODE_RAW_1
        case "[Ljava.lang.String;" => TCODE_CHARACTER_1
        case "[[I" => TCODE_INT_2
        case "[[D" => TCODE_DOUBLE_2
        case "[[Z" => TCODE_LOGICAL_2
        case "[[B" => TCODE_RAW_2
        case "[[Ljava.lang.String;" => TCODE_CHARACTER_2
        case _ => throw new RuntimeException("Unsupported array type.")
      }
    } else {
      c.getName match {
        case "java.lang.Integer" => TCODE_INT_0
        case "java.lang.Double" => TCODE_DOUBLE_0
        case "java.lang.Boolean" => TCODE_LOGICAL_0
        case "java.lang.Byte" => TCODE_RAW_0
        case "java.lang.String" => TCODE_CHARACTER_0
        case "org.ddahl.rscala.RObject" => TCODE_ROBJECT
        case o => throw new RuntimeException("Unsupported type: <"+o+">")
      }
    }
    if ( tipe == TCODE_ROBJECT ) Datum(any.asInstanceOf[RObject].x, tipe, None)
    else Datum(any, tipe, None)
  }

}

