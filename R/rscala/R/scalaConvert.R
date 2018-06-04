scalaConvert <- function(x, interpreter=findScalaInstance()) {
  s <- interpreter
  if ( inherits(x,"ScalaCachedReference") || inherits(x,"ScalaInterpreterReference") ) {
    result <- s['x']  %~% 'x'
    if ( inherits(result,"ScalaCachedReference") ) stop("Failed to convert object.")
    result
  } else {
    result <- s['x'] %.~% 'x'
    if ( result$type == "org.ddahl.rscala.EphemeralReference" ) stop("Failed to convert object.")
    result
  }
}

scalaConvert.data.frame <- function(x, interpreter=findScalaInstance()) {
  s <- interpreter
  if ( inherits(x,"ScalaCachedReference") || inherits(x,"ScalaInterpreterReference") ) {
    names <- setdiff(x$keys()$toArray(),".rownames")
    results <- as.data.frame(lapply(names,function(name) { y <- x; s['y'] %!% 'y(name)._1' }))
    dimnames(results) <- list(x$apply(".rownames")$"_1"(),names)
    results
  } else {
    s %.~% '
      val names = R.evalS1("names(x)")
      val nCols = names.length
      var result = scala.collection.immutable.ListMap[String,(Any,String)]().withDefaultValue(null)
      result = result + ((".rownames",R.eval("rownames(x)",false,false)))
      for ( i <- 0 until nCols ) {
        result = result + ((names(i),R.eval(s"""
          y <- x[,${i+1}]
          if ( is.factor(y) ) levels(y)[y] else y
        """,false,false)))
      }
      result
    '
  }
}

