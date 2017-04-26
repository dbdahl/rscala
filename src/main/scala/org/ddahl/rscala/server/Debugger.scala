package org.ddahl.rscala.server

class Debugger(val value: Boolean, out: java.io.PrintWriter, label: String, withTime: Boolean) {

  private val maxOutputLength = 2000

  def timestamp = new java.sql.Timestamp(System.currentTimeMillis).toString

  def msg(msg: String) = {
    val pretext = "DEBUG ("+label+") "+(if ( withTime ) timestamp else "")+": "
    if ( msg.length > maxOutputLength ) out.println(pretext+msg.substring(0,maxOutputLength-3)+"...")
    else out.println(pretext+msg)
  }

}

