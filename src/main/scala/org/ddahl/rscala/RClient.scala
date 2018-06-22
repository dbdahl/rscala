package org.ddahl.rscala

import Protocol._

class RClient(server: Server) {

  private def eval[A](template: String, values: Seq[Datum]): A = {
    server.report(Datum(values.length, TCODE_CALLBACK, Some(template)))
    values.foreach(server.report)
    server.run(true)
    server.conduit.pop[A]
  }

  def evalI0(template: String, values: Datum*): Int                   = eval[Int]                   (template, values)
  def evalI1(template: String, values: Datum*): Array[Int]            = eval[Array[Int]]            (template, values)
  def evalI2(template: String, values: Datum*): Array[Array[Int]]     = eval[Array[Array[Int]]]     (template, values)

  def evalD0(template: String, values: Datum*): Double                = eval[Double]                (template, values)
  def evalD1(template: String, values: Datum*): Array[Double]         = eval[Array[Double]]         (template, values)
  def evalD2(template: String, values: Datum*): Array[Array[Double]]  = eval[Array[Array[Double]]]  (template, values)

  def evalL0(template: String, values: Datum*): Boolean               = eval[Boolean]               (template, values)
  def evalL1(template: String, values: Datum*): Array[Boolean]        = eval[Array[Boolean]]        (template, values)
  def evalL2(template: String, values: Datum*): Array[Array[Boolean]] = eval[Array[Array[Boolean]]] (template, values)

  def evalR0(template: String, values: Datum*): Byte                  = eval[Byte]                  (template, values)
  def evalR1(template: String, values: Datum*): Array[Byte]           = eval[Array[Byte]]           (template, values)
  def evalR2(template: String, values: Datum*): Array[Array[Byte]]    = eval[Array[Array[Byte]]]    (template, values)

  def evalS0(template: String, values: Datum*): String                = eval[String]                (template, values)
  def evalS1(template: String, values: Datum*): Array[String]         = eval[Array[String]]         (template, values)
  def evalS2(template: String, values: Datum*): Array[Array[String]]  = eval[Array[Array[String]]]  (template, values)

}

