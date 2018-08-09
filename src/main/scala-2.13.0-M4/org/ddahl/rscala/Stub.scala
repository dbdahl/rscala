package org.ddahl.rscala

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.shell.{ShellConfig, ReplReporterImpl}
import scala.tools.nsc.{Settings, NewLinePrintWriter}
import java.io.PrintWriter

object Stub {

  val Success = scala.tools.nsc.interpreter.Results.Success

  def mkIMain(settings: Settings, printWriter: PrintWriter): IMain = {
    val writer = new NewLinePrintWriter(printWriter, true)
    val reporter = new ReplReporterImpl(ShellConfig(settings),settings,writer)
    reporter.togglePrintResults()
    new IMain(settings, reporter)
  }

}

