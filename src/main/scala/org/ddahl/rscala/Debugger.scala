package org.ddahl.rscala

class Debugger(var on: Boolean, out: java.io.PrintWriter, val label: String, val withTime: Boolean) {

  private val maxOutputLength = 2000

  def timestamp(): String = new java.sql.Timestamp(System.currentTimeMillis).toString

  def apply(msg: String): Unit = {
    if ( on ) {
      val pretext = "DEBUG (" + label + ") " + (if (withTime) timestamp else "") + ": "
      if (msg.length > maxOutputLength) out.println(pretext + msg.substring(0, maxOutputLength - 3) + "...")
      else out.println(pretext + msg)
    }
  }

}
