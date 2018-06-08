package org.ddahl.rscala2.server

object Protocol {

  // commands

  val PCODE_EXIT   = 0
  val PCODE_PUSH   = 1
  val PCODE_INVOKE_WITH_NAMES = 2
  val PCODE_INVOKE_WITHOUT_NAMES = 3

  // results

  val RESULT_OKAY = 0
  val RESULT_COMPILATION_ERROR = 1

  // sizes

  val BYTES_PER_INT = 4
  val BYTES_PER_DOUBLE = 8

  // type codes
  val TCODE_INT_0 = 1000
  val TCODE_INT_1 = 1001
  val TCODE_DOUBLE_0 = 1010
  val TCODE_DOUBLE_1 = 1011
  val TCODE_LOGICAL_0 = 1020
  val TCODE_LOGICAL_1 = 1021
  val TCODE_RAW_0 = 1030
  val TCODE_RAW_1 = 1031
  val TCODE_CHARACTER_0 = 1040
  val TCODE_CHARACTER_1 = 1041

  // type mapper

  val typeMapper = Map(
    TCODE_INT_0 -> "Int",
    TCODE_INT_1 -> "Array[Int]",
    TCODE_DOUBLE_0 -> "Double",
    TCODE_DOUBLE_1 -> "Array[Double]",
    TCODE_LOGICAL_0 -> "Boolean",
    TCODE_LOGICAL_1 -> "Array[Boolean]",
    TCODE_RAW_0 -> "Byte",
    TCODE_RAW_1 -> "Array[Byte]",
    TCODE_CHARACTER_0 -> "String",
    TCODE_CHARACTER_1 -> "Array[String]",
  )

}

