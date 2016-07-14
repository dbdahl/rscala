library(rscala)
s <- scala()

a <- s$def('','R.evalD0("rnorm(1)")')
system.time(sapply(1:1000,function(i) a()))

b <- s$def('','for ( i <- 0 until 10000 ) R.evalD0("rnorm(1)")')

system.time(b())



