library(rscala)

scala()

sampler <- function() {
  list(mean=rnorm(1),prob=rbeta(1,1,1))
}

mm <- function(n) s(sf=s-sampler,n=as.integer(n[1])) * '
  var time = System.nanoTime()
  R.eval("mySF <- %-",sf)          // Set it once.  See below.
  List.tabulate(n) { i =>
    val newTime = System.nanoTime()
    val lapse = newTime - time
    time = newTime
    println(lapse+" : "+i)
    //R.evalObject("%-()", sf)     // This slows down at some point until the function finishes.
    R.evalObject("mySF()")         // So its better to do this.  See above.
  }
'

system.time(mm(300))
system.time(mm(300))
system.time(mm(300))
system.time(mm(1000))
system.time(mm(100000))
