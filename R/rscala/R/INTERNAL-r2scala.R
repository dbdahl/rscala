r2scala <- function(x, showCode=FALSE) {
  if ( length(x) == 0 ) return("")
  typeof  <- sapply(x,function(y) typeof(y))
  classes <- sapply(x,function(y) class(y))
  strings <- sapply(x,function(y) toString(y))
  if ( showCode ) {
    cat("<<\n")
    print(typeof)
    print(classes)
    print(strings)
    cat(">>\n")
  }
  if ( ( typeof[1] == "symbol" ) && ( classes[1] == "name" ) ) {
    if ( strings[1] == "{" ) paste0("{\n",paste0(sapply(x[-1],r2scala,showCode=showCode),collapse="\n"),"\n}")
    else if ( strings[1] == "<-" ) paste0("val ",r2scala(x[[2]],showCode=showCode)," = ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == ":" ) paste0("Array.range(",r2scala(x[[2]],showCode=showCode),".toInt,",r2scala(x[[3]],showCode=showCode),".toInt+1)")
    else if ( strings[1] == "(" ) paste0("(",r2scala(x[[2]],showCode=showCode),")")
    else if ( strings[1] == "+" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode=showCode)," + ",r2scala(x[[3]],showCode=showCode)) else paste0("+",r2scala(x[[2]],showCode=showCode))
    else if ( strings[1] == "-" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode=showCode)," - ",r2scala(x[[3]],showCode=showCode)) else paste0("-",r2scala(x[[2]],showCode=showCode))
    else if ( strings[1] == "*" ) paste0(r2scala(x[[2]],showCode=showCode)," * ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "/" ) paste0(r2scala(x[[2]],showCode=showCode),".toDouble / ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "%%" ) paste0(r2scala(x[[2]],showCode=showCode)," % ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "^" ) paste0("pow(",r2scala(x[[2]],showCode=showCode),",",r2scala(x[[3]],showCode=showCode),")")
    else if ( ! is.list(x) && is.symbol(x) ) paste0(strings[1])
    else if ( ( strings[1] == "I" ) && ( length(x) == 2 ) && ( typeof[2] == "character" ) ) paste0(strings[2])
    else paste0(strings[1],"(",paste0(sapply(x[-1],r2scala,showCode=showCode),collapse=","),")")
  }
  else if ( typeof[1] == "integer" ) paste0("{",strings[1],":Int}")
  else if ( typeof[1] == "double" ) paste0("{",strings[1],":Double}")
  else if ( typeof[1] == "character" ) paste0('{"',strings[1],'":String}')
  else stop("33")
}
