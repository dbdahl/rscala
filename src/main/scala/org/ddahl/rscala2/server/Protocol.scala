package org.ddahl.rscala2.server

object Protocol {

  // commands

  val PCODE_EXIT  = 0
  val PCODE_PUSH  = 1
  val PCODE_POP   = 2

  // sizes

  val BYTES_PER_INT = 4
  val BYTES_PER_DOUBLE = 8

  // integer

  val TCODE_INT_0 = 1000
  val SSNIPPET_INT_0 = "RScalaStack.pop().asInstanceOf[Int]"

  val TCODE_INT_1 = 1001
  val SSNIPPET_INT_1 = "RScalaStack.pop().asInstanceOf[Array[Int]]"

  // double

  val TCODE_DOUBLE_0 = 1010
  val SSNIPPET_DOUBLE_0 = "RScalaStack.pop().asInstanceOf[Double]"

  val TCODE_DOUBLE_1 = 1011
  val SSNIPPET_DOUBLE_1 = "RScalaStack.pop().asInstanceOf[Array[Double]]"

  // logical

  val TCODE_LOGICAL_0 = 1020
  val SSNIPPET_LOGICAL_0 = "RScalaStack.pop().asInstanceOf[Boolean]"

  val TCODE_LOGICAL_1 = 1021
  val SSNIPPET_LOGICAL_1 = "RScalaStack.pop().asInstanceOf[Array[Boolean]]"

  // raw

  val TCODE_RAW_0 = 1030
  val SSNIPPET_RAW_0 = "RScalaStack.pop().asInstanceOf[Byte]"

  val TCODE_RAW_1 = 1031
  val SSNIPPET_RAW_1 = "RScalaStack.pop().asInstanceOf[Array[Byte]]"

  // character

  val TCODE_CHARACTER_0 = 1040
  val SSNIPPET_CHARACTER_0 = "RScalaStack.pop().asInstanceOf[String]"

  val TCODE_CHARACTER_1 = 1041
  val SSNIPPET_CHARACTER_1 = "RScalaStack.pop().asInstanceOf[Array[String]]"

}

