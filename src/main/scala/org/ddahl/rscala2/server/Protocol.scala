package org.ddahl.rscala2.server

object Protocol {

  // Since Scala uses signed bytes and R uses unsigned bytes, stay between 0 and 127 to avoid extra work.

  // commands

  val PCODE_EXIT                  = 10.toByte
  val PCODE_PUSH                  = 11.toByte
  val PCODE_INVOKE_WITH_NAMES     = 12.toByte
  val PCODE_INVOKE_WITHOUT_NAMES  = 13.toByte
  val PCODE_INVOKE_WITH_REFERENCE = 14.toByte
  val PCODE_ECHO                  = 15.toByte
  val PCODE_GARBAGE_COLLECT       = 16.toByte

  // type codes

  val TCODE_INT_0        = 50.toByte
  val TCODE_INT_1        = 51.toByte
  val TCODE_DOUBLE_0     = 52.toByte
  val TCODE_DOUBLE_1     = 53.toByte
  val TCODE_LOGICAL_0    = 54.toByte
  val TCODE_LOGICAL_1    = 55.toByte
  val TCODE_RAW_0        = 56.toByte
  val TCODE_RAW_1        = 57.toByte
  val TCODE_CHARACTER_0  = 58.toByte
  val TCODE_CHARACTER_1  = 59.toByte
  val TCODE_UNIT         = 60.toByte
  val TCODE_REFERENCE    = 70.toByte
  val TCODE_ERROR_DEF    = 80.toByte
  val TCODE_ERROR_INVOKE = 81.toByte

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
    TCODE_UNIT -> "Unit"
  )

  val typeMapper2 = typeMapper.map(_.swap)

}

