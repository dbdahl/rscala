package org.ddahl.rscala

class Debugger(var debug: Boolean = false, out: java.io.PrintStream) {

  private val maxOutputLength = 2000

  def timestamp = new java.sql.Timestamp(System.currentTimeMillis()).toString

  def msg(msg: String) = {
    val label = "DEBUG "+timestamp+": "
    if ( msg.length > maxOutputLength ) out.println(label+msg.substring(0,maxOutputLength-3)+"...")
    else out.println(label+msg)
  }

}

