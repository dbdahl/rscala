package org.ddahl.rscala

import Protocol._

class RClient(server: Server) {

  def evalI0(template: String, values: Any*): Int                   = eval[Int]                   (template, values)
  def evalI1(template: String, values: Any*): Array[Int]            = eval[Array[Int]]            (template, values)
  def evalI2(template: String, values: Any*): Array[Array[Int]]     = eval[Array[Array[Int]]]     (template, values)

  def evalD0(template: String, values: Any*): Double                = eval[Double]                (template, values)
  def evalD1(template: String, values: Any*): Array[Double]         = eval[Array[Double]]         (template, values)
  def evalD2(template: String, values: Any*): Array[Array[Double]]  = eval[Array[Array[Double]]]  (template, values)

  def evalL0(template: String, values: Any*): Boolean               = eval[Boolean]               (template, values)
  def evalL1(template: String, values: Any*): Array[Boolean]        = eval[Array[Boolean]]        (template, values)
  def evalL2(template: String, values: Any*): Array[Array[Boolean]] = eval[Array[Array[Boolean]]] (template, values)

  def evalR0(template: String, values: Any*): Byte                  = eval[Byte]                  (template, values)
  def evalR1(template: String, values: Any*): Array[Byte]           = eval[Array[Byte]]           (template, values)
  def evalR2(template: String, values: Any*): Array[Array[Byte]]    = eval[Array[Array[Byte]]]    (template, values)

  def evalS0(template: String, values: Any*): String                = eval[String]                (template, values)
  def evalS1(template: String, values: Any*): Array[String]         = eval[Array[String]]         (template, values)
  def evalS2(template: String, values: Any*): Array[Array[String]]  = eval[Array[Array[String]]]  (template, values)

  private def eval[A](template: String, values: Seq[Any]): A = {
    server.report(Datum(values.length, TCODE_CALLBACK, Some(template)))
    values.foreach(v => server.report(any2Datum(v)))
    server.run(true)
    server.conduit.pop[A]
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
        case _ => TCODE_REFERENCE
      }
    } else {
      c.getName match {
        case "java.lang.Integer" => TCODE_INT_0
        case "java.lang.Double" => TCODE_DOUBLE_0
        case "java.lang.Boolean" => TCODE_LOGICAL_0
        case "java.lang.Byte" => TCODE_RAW_0
        case "java.lang.String" => TCODE_CHARACTER_0
        case _ => TCODE_REFERENCE
      }
    }
    Datum(any, tipe, None)
  }

}

