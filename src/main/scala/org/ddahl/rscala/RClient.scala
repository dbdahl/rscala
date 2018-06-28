package org.ddahl.rscala

import Protocol._

class RClient() {

  private[rscala] var server: Server = null

  def eval  (template: String, values: Any*): Unit                  = evalWithoutResult                     (template, values)

  def evalI0(template: String, values: Any*): Int                   = evalWithResult[Int]                   (template, values)
  def evalI1(template: String, values: Any*): Array[Int]            = evalWithResult[Array[Int]]            (template, values)
  def evalI2(template: String, values: Any*): Array[Array[Int]]     = evalWithResult[Array[Array[Int]]]     (template, values)

  def evalD0(template: String, values: Any*): Double                = evalWithResult[Double]                (template, values)
  def evalD1(template: String, values: Any*): Array[Double]         = evalWithResult[Array[Double]]         (template, values)
  def evalD2(template: String, values: Any*): Array[Array[Double]]  = evalWithResult[Array[Array[Double]]]  (template, values)

  def evalL0(template: String, values: Any*): Boolean               = evalWithResult[Boolean]               (template, values)
  def evalL1(template: String, values: Any*): Array[Boolean]        = evalWithResult[Array[Boolean]]        (template, values)
  def evalL2(template: String, values: Any*): Array[Array[Boolean]] = evalWithResult[Array[Array[Boolean]]] (template, values)

  def evalR0(template: String, values: Any*): Byte                  = evalWithResult[Byte]                  (template, values)
  def evalR1(template: String, values: Any*): Array[Byte]           = evalWithResult[Array[Byte]]           (template, values)
  def evalR2(template: String, values: Any*): Array[Array[Byte]]    = evalWithResult[Array[Array[Byte]]]    (template, values)

  def evalS0(template: String, values: Any*): String                = evalWithResult[String]                (template, values)
  def evalS1(template: String, values: Any*): Array[String]         = evalWithResult[Array[String]]         (template, values)
  def evalS2(template: String, values: Any*): Array[Array[String]]  = evalWithResult[Array[Array[String]]]  (template, values)

  private def evalWithoutResult[A](template: String, values: Seq[Any]): Unit = {
    evalEngine(template + "; NULL",values)
    server.conduit.pop[Any]
  }

  private def evalWithResult[A](template: String, values: Seq[Any]): A = {
    evalEngine(template,values)
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
        case _ => throw new RuntimeException("Unsupported type.")
      }
    }
    Datum(any, tipe, None)
  }

}

