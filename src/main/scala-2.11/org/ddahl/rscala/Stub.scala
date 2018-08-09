package org.ddahl.rscala

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.Settings
import java.io.PrintWriter

object Stub {

  val Success = scala.tools.nsc.interpreter.IR.Success

  def mkIMain(settings: Settings, printWriter: PrintWriter): IMain = {
    val intp = new IMain(settings, printWriter)
    val iloop = new ILoop()
    iloop.intp = intp
    iloop.verbosity()
    intp
  }

}

