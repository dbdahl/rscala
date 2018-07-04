library(rscala)

scala(JARs="renjin-script-engine-0.9.2648-jar-with-dependencies.jar")
s

s + '
  import javax.script._
  import org.renjin.script._
  import org.renjin.sexp._
'

se <- s$.new_RenjinScriptEngineFactory()$getScriptEngine()

se$eval("df <- data.frame(x=1:10, y=(1:10)+rnorm(n=10))")
se$eval("print(df)")

evalRenjin <- function(snippet) s(se=se,snippet=snippet) * '
  val ref = se.eval(snippet).asInstanceOf[DoubleVector]
  Array.tabulate[Double](ref.length) { i =>
    ref.getElementAsDouble(i)
  }
'

evalRenjinMany <- function(snippet,times) s(se=se,snippet=snippet,times=as.integer(times[1])) * '
  Array.fill(times) {
    val ref = se.eval(snippet).asInstanceOf[DoubleVector]
    Array.tabulate[Double](ref.length) { i =>
      ref.getElementAsDouble(i)
    }
  }
'
evalRenjinMany("c(1,2,3)",100)

evalRscalaMany <- function(snippet,times) s(snippet=snippet,times=as.integer(times[1])) * '
  Array.fill(times) {
    R.evalD1(snippet)
  }
'
evalRscalaMany("c(1,2,3)",100)

evalNativeMany <- function(snippet,times) t(sapply(1:times,function(i) eval(parse(text=snippet))))

library(microbenchmark)
microbenchmark(
  evalRenjinMany("c(1,2,3)",1000)
  ,
  evalRscalaMany("c(1,2,3)",1000)
  ,
  evalNativeMany("c(1,2,3)",1000)
  ,times=10)



library(microbenchmark)
microbenchmark(
  evalRenjin("c(1+2,4)")
  ,
  evalRenjin("rnorm(1000)")
  ,
  s$R.evalD1("c(1+2,4)")
  ,
  s$R.evalD1("rnorm(1000)")
  ,times=1000)


r <- se$eval("serialize(function(i) 2+i,NULL)")$'asInstanceOf[RawVector]'()
f <- unserialize(r$toByteArrayUnsafe())
f(5)


