library(rscala)

scala()

sampler <- function() {
  list(mean=rnorm(1),prob=rbeta(1,1,1))
}

mm <- function(n) s(sf=s-sampler,n=as.integer(n[1])) * '
  var time = System.nanoTime()
  List.tabulate(n) { i =>
    if ( i % 100 == 0 ) R.eval("gc()")  // The need for this is weird!
    val newTime = System.nanoTime()
    val lapse = newTime - time
    time = newTime
    println(lapse+" : "+i)
    R.evalObject("%-()",sf)
  }
'

system.time(mm(1000))
system.time(mm(1000))
system.time(mm(100000))

