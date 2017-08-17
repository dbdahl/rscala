#!/bin/bash

CMD="cat(rscala::.rscalaJar(\"$RSCALA_SCALA_VERSION\"))"
CP=RserveEngine.jar:REngine.jar:$(R --slave -e "$CMD")
exec "$SCALA_HOME"/bin/scala -nc -cp "$CP" "$0" "$@"
scala -nc -cp "$CP"

!#

println(util.Properties.versionNumberString)

import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

val R = org.ddahl.rscala.RClient(debug=false)

val prob1 = 0.70
val prob2 = 0.35
val n = 100
val alpha = 0.05
val nIntervals = 1000
val nSamples = 1000
val truth = -1.75
val sampler = "rnorm"

def quantile(sorted: Array[Double], p: Double) = {
  val i = ((sorted.length-1)*p).asInstanceOf[Int]
  val delta = (sorted.length-1)*p - i
  ( 1 - delta ) * sorted(i) + delta * sorted(i+1)
}

def statistic(x: Array[Double]) = {
  scala.util.Sorting.quickSort(x)
  quantile(x,prob1) / quantile(x,prob2)
}

def resample(x: Array[Double], rng: Random) = Array.fill(x.length) {
  x(rng.nextInt(x.length))
}

def ciContains(x: Array[Double], rng: Random) = {
  val bs = Array.fill(nSamples) { statistic(resample(x, rng)) }
  scala.util.Sorting.quickSort(bs)
  ( quantile(bs, alpha/2) <= truth ) && ( truth <= quantile(bs, 1-alpha/2) )
}

val coverage = Await.result( Future.sequence( List.fill(nIntervals) {
  Future {
    val dataset = R.invokeD1(sampler, n)
    val rng = new Random(R.invokeI0("runif", 1, -Int.MaxValue, Int.MaxValue))
    ciContains(dataset, rng)
  }
}), concurrent.duration.Duration.Inf).count(identity) / nIntervals.toDouble

