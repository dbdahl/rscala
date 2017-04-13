#!/bin/bash

CMD="cat(rscala::.rscalaJar(\"$RSCALA_SCALA_VERSION\"))"
CP=$(R --slave -e "$CMD")
exec "$SCALA_HOME"/bin/scala -nc -cp "$CP" "$0" "$@"

!#

println(util.Properties.versionNumberString)

val R = org.ddahl.rscala.RClient(sys.env("RSCALA_SERIALIZE").toUpperCase == "TRUE")

try {
  R.eval("library(dfasdf)")  // Tests for UTF-8 strings because of curly single quotes.
} catch {
  case _: Throwable => "Okay"
}

val a1 = Array(1,2,3,4)
R.set("a",a1)
R.set("a",67,"7")
R.get("a")

R.eval("names <- list()")
R.set("names","Dahl","'David'")
R.evalS0("names[['David']]")

val a2 = Array("a","b","c","d")
R.set("a",a2)
R.get("a")

R.eval("a <- 0")
R.eval("cat(a+8,'\n')")

// R.eval("plot(rnorm(100))")

R.eval("rnorm(10)")
R.eval("rnorm(10)",false)
R.eval("rnorm(10)",false).asInstanceOf[Array[Double]]
R.evalD1("rnorm(10)")

R.eval("d <- list(a=4,b=5)")

R.eval("a <- rnorm(1000)")
def timer(a: => Unit) = {
  val now = System.nanoTime
  a
  val micros = (System.nanoTime - now) / 1000
  println("%d microseconds".format(micros))
}

println("Burnin the hotspot compiler.")
timer{ for ( i <- 0 until 100 ) R.evalD1("a") }
timer{ for ( i <- 0 until 100 ) R.getD1("a") }

println("Get then eval.")
timer{ for ( i <- 0 until 100 ) R.getD1("a") }
timer{ for ( i <- 0 until 100 ) R.evalD1("a") }

println("Eval then get.")
timer{ for ( i <- 0 until 100 ) R.evalD1("a") }
timer{ for ( i <- 0 until 100 ) R.getD1("a") }

println("Eval then get.")
timer{ for ( i <- 0 until 100 ) R.evalD1("a") }
timer{ for ( i <- 0 until 100 ) R.getD1("a") }

println("Get then eval.")
timer{ for ( i <- 0 until 100 ) R.getD1("a") }
timer{ for ( i <- 0 until 100 ) R.evalD1("a") }


R.a._1
R.getD1("a")




R.set("a",a2)
R.eval("cat(paste(a,collapse='\n'))")

val a3 = Array[Double](1,2,5)
R.set("bb",a3)
R.eval("cat(sum(bb+8),'\n')")

R.set("aa",2)
R.eval("cat(sum(aa+8),'\n')")

val (a4,t) = R.get("aa")

R.set("david",Array(true,false,true))
R.get("david")

R.aa
R.name = "David"
val (v,t2) = R.name
R.name._2
R.name._1.asInstanceOf[Array[String]](0)

R.a = 4

R.eval("3+8")
2*R.evalI0("3+8")

R.eval("4")
R.eval("print(.rscala.last.value)")

R.eval("""
  a <- matrix(as.integer(0:11),nrow=3)
  print(a)
""")
R.getI2("a")
R.getD2("a")
R.getB2("a")
R.getS2("a")

R.eval("""
  a <- matrix(as.double(0:11),nrow=3)
  print(a)
""")
R.getI2("a")
R.getD2("a")
R.getB2("a")
R.getS2("a")

R.eval("""
  a <- matrix(as.logical(0:11),nrow=3)
  print(a)
""")
R.getI2("a")
R.getD2("a")
R.getB2("a")
R.getS2("a")

R.eval("""
  a <- matrix(as.character(0:11),nrow=3)
  print(a)
""")
R.getI2("a")
R.getD2("a")
R.getB2("a")
R.getS2("a")

R.eval("""
  a <- as.integer(0:4)
  print(a)
""")
R.getI1("a")
R.getD1("a")
R.getB1("a")
R.getS1("a")
R.getI0("a")
R.getD0("a")
R.getB0("a")
R.getS0("a")

R.eval("""
  a <- as.double(0:4)
  print(a)
""")
R.getI1("a")
R.getD1("a")
R.getB1("a")
R.getS1("a")
R.getI0("a")
R.getD0("a")
R.getB0("a")
R.getS0("a")

R.eval("""
  a <- as.logical(0:4)
  print(a)
""")
R.getI1("a")
R.getD1("a")
R.getB1("a")
R.getS1("a")
R.getI0("a")
R.getD0("a")
R.getB0("a")
R.getS0("a")

R.eval("""
  a <- as.character(0:4)
  print(a)
""")
R.getI1("a")
R.getD1("a")
R.getB1("a")
R.getS1("a")
R.getI0("a")
R.getD0("a")
R.getB0("a")
R.getS0("a")

R.eval("cat('Done')")

R.exit()

println("Done")

// LF RSCALA_SERIALIZE=TRUE scala -cp $(R --slave -e 'cat(rscala::.rscalaJar("2.12"))')
