a <- quote({
 m <- 1 + 3
 rnorm(1,mean=mean,sd=2)
})

a <- quote( m <- 1 )

r2scala <- function(x) {
  classes <- sapply(x,function(y) class(y))
  strings <- sapply(x,function(y) toString(y))
  if ( length(x) == 1 ) {
    if ( classes[1] == "name" ) paste0(strings[1])
    else if ( classes[1] == "numeric" ) paste0("{ ",strings[1],": Double }")
    else if ( classes[1] == "integer" ) paste0("{ ",strings[1],": Int }")
    else if ( classes[1] == "character" ) paste0('{ "',strings[1],'": String }')
    else { cat("2\n"); browser() }
  }
  else if ( length(x) == 2 ) {
    if ( classes[1] == "name" ) {
      if ( strings[1] == "(" ) paste0("( ",r2scala(x[[2]])," )")
      else paste0(strings[1],"(",r2scala(x[[2]]),")")
    }
  }
  else if ( length(x) == 3 ) {
    if ( classes[1] == "name" ) {
      if ( ( strings[1] == "<-" ) && ( classes[2] == "name" ) ) {
        paste0("val ", strings[2], " = ", r2scala(x[[3]]))
      } else if ( strings[1] == "+" ) {
        paste0(r2scala(x[[2]]), " + ", r2scala(x[[3]]))
      } else if ( strings[1] == "-" ) {
        paste0(r2scala(x[[2]]), " - ", r2scala(x[[3]]))       
      } else if ( strings[1] == "*" ) {
        paste0(r2scala(x[[2]]), " * ", r2scala(x[[3]]))       
      } else if ( strings[1] == "/" ) {
        paste0(r2scala(x[[2]]), " / ", r2scala(x[[3]]))
      } else paste0(strings[1],"(",r2scala(x[[2]]),",",r2scala(x[[3]]),")")
    } else { cat("4\n"); browser() }
  } else { cat("5\n"); browser() }
}

assert <- function(x) {
  if ( !x ) stop()
}

r2scala <- function(x) {
  typeof  <- sapply(x,function(y) typeof(y))
  classes <- sapply(x,function(y) class(y))
  strings <- sapply(x,function(y) toString(y))
  cat("--\n")
  
  if ( typeof[1] == "symbol" ) paste(strings[1])
  else if ( classes[1] == "name" ) {
    if ( strings[1] == "<-" ) {
      assert(length(x)==3)
      paste0("val ", r2scala(x[[2]]), " = ", r2scala(x[[3]]))
    } else if ( strings[1] == "(" ) {
      assert(length(x)==2)
      paste0("( ",r2scala(x[[2]])," )")
    } else if ( strings[1] == "/" ) {
      assert(length(x)==3)
      paste0(r2scala(x[[2]]),".toDouble",strings[1],r2scala(x[[3]]))
    } else if ( strings[1] %in% c("+","-","*") ) {
      assert(length(x)==3)
      paste0(r2scala(x[[2]]),strings[1],r2scala(x[[3]]))
    } else {
      assert(length(x)==1)
      strings[1]
    }
  }
  else if ( classes[1] == "numeric" ) paste0("{",strings[1],":Double}")
  else if ( classes[1] == "integer" ) paste0("{",strings[1],":Int}")
  else if ( classes[1] == "character" ) paste0('{"',strings[1],'":String}')
  else { cat("2\n"); browser() }
}

r2scala(quote(m <- 1L))
r2scala(quote(davidDahl <- 1 + 3L))
r2scala(quote(davidDahl <- 1L / 3L))
r2scala(quote(davidDahl <- 1 + 3 / ( 2 * 4 )))
r2scala(quote(rbob(1,3L)))

r2scala(quote(rbob))
r2scala(quote(rbob()))

a <- quote(rbob)
b <- quote(rbob())




asses
r2scala <- function(x) {
  if ( e == "{" ) {
    "locally {" + r2scala(e[-1]) + "}"
  } else for ( e in x ) {
    if ( e == "<-" ) 
  }
}



length(a)
a[[1]]
b2 <- a[[2]]
b3 <- a[[3]]

b <- quote(a)

a[[1]]
class(a[[2]][[2]])
as.list(a)

