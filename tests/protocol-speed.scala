#!/bin/bash

CP="RserveEngine.jar:REngine.jar:$(R --slave -e 'cat(rscala::rscalaJar("2.11"))')"
exec scala -cp "$CP" "$0" "$@"
scala -cp "$CP"

!#

class TimeMonitor (private var sum: Long, private var invocations: Long) {

  def total = sum

  def nInvocations = invocations

  def apply[B](block: => B): B = {
    invocations += 1
    val t0 = System.nanoTime()
    val result = block // call-by-name
    sum += System.nanoTime() - t0
    result
  }

  def /(d: TimeMonitor) = sum.toDouble / d.total

  def +(d: TimeMonitor) = new TimeMonitor(sum + d.sum, invocations + d.invocations)

  def rate = sum.toDouble / invocations

  override def toString = "%d invocations in %4.4f seconds".format(invocations, sum / 1e9)

}

object TimeMonitor {

  def apply() = new TimeMonitor(0L, 0L)

}

import scala.annotation.tailrec
@tailrec
def repeat(n: Long)(f: => Unit): Unit = {
  if (n > 0) {
    f
    repeat(n - 1)(f)
  }
}



val timerRserve = TimeMonitor()
val c1 = try {
  new org.rosuda.REngine.Rserve.RConnection()
} catch {
  case e: Throwable =>
    scala.Console.err.println("Did you start the server?  Do so using: R CMD Rserve")
    sys.exit(1)
}
repeat(10000) {
  timerRserve {
    assert( c1.eval("1").asDouble == 1.0 )
  }
}
println(timerRserve)



val timerrscala = TimeMonitor()
val c2 = org.ddahl.rscala.RClient()
repeat(10000) {
  timerrscala {
    assert( c2.evalD0("1") == 1.0 )
  }
}
println(timerrscala)


