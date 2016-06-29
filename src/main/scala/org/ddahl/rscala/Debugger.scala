package org.ddahl.rscala

import scala.Console.originalOut

class Debugger(var debug: Boolean = false) {

  private val out = try {
    if ( originalOut != null ) originalOut
    else Console.out
  } catch {
    case _: Throwable =>
      Console.out
  }

  private val maxOutputLength = 2000

  def msg(msg: String) = {
    if ( msg.length > maxOutputLength ) out.println("DEBUG: "+msg.substring(0,maxOutputLength-3)+"...")
    else out.println("DEBUG: "+msg)
  }

}

