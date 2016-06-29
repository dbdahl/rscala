package org.ddahl.rscala

class Debugger(var debug: Boolean = false) {

  private val maxOutputLength = 2000

  def msg(msg: String) = {
    if ( msg.length > maxOutputLength ) println("DEBUG: "+msg.substring(0,maxOutputLength-3)+"...")
    else println("DEBUG: "+msg)
  }

}

