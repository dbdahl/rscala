package org.ddahl.rscala2.server

object Protocol {

  // commands

  val PCODE_EXIT                 = 0.toByte
  val PCODE_PUSH                 = 1.toByte
  val PCODE_INVOKE_WITH_NAMES    = 2.toByte
  val PCODE_INVOKE_WITHOUT_NAMES = 3.toByte

  // type codes

  val TCODE_INT_0       = 50.toByte
  val TCODE_INT_1       = 51.toByte
  val TCODE_DOUBLE_0    = 52.toByte
  val TCODE_DOUBLE_1    = 53.toByte
  val TCODE_LOGICAL_0   = 54.toByte
  val TCODE_LOGICAL_1   = 55.toByte
  val TCODE_RAW_0       = 56.toByte
  val TCODE_RAW_1       = 57.toByte
  val TCODE_CHARACTER_0 = 58.toByte
  val TCODE_CHARACTER_1 = 59.toByte
  val TCODE_ERROR       = 60.toByte

  // sizes

  val BYTES_PER_INT = 4
  val BYTES_PER_DOUBLE = 8

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

