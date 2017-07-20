#!/bin/bash

CMD="cat(rscala::.rscalaJar(\"$RSCALA_SCALA_VERSION\"))"
CP=$(R --slave -e "$CMD")
exec "$SCALA_HOME"/bin/scala -nc -cp "$CP" "$0" "$@"

!#

println(util.Properties.versionNumberString)

import org.ddahl.rscala.RClient
val R = RClient("R", serializeOutput=sys.env("RSCALA_SERIALIZE").toUpperCase == "TRUE")

class WorkerBee(val label: Int, val iterations: Int, val R: RClient) extends Runnable {

  def run() : Unit = {
    for (i <- 0 until iterations) {
      // print(label+" ")
      R.eval("x <- x + 1")
    }
  }
    
}

val nThreads = 10
val nIterationsPerThread = 1000

R.set("x",0)

val g = new ThreadGroup("WorkerBees")
val threads = (0 until nThreads).map( i => {
  new Thread(g, new WorkerBee(i, nIterationsPerThread, R))
})
threads.foreach(_.start)
threads.foreach(_.join)
 
assert(R.getI0("x") == nThreads*nIterationsPerThread)

R.ping
R.exit
R.ping

// LF RSCALA_SERIALIZE=TRUE scala -cp $(R --slave -e 'cat(rscala::.rscalaJar("2.12"))')
