r2scala <- function(x, showCode=FALSE, symbolEnv=new.env(parent=emptyenv())) {
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
    if ( ! is.list(x) && is.symbol(x) ) {
      if ( strings[1] == "val" ) "_val" else strings[1]
    }
    else if ( strings[1] == "{" ) paste0("{\n",paste0(sapply(x[-1],r2scala,showCode,symbolEnv),collapse="\n"),"\n}")
    else if ( strings[1] == "[" ) paste0(r2scala(x[[2]],showCode,symbolEnv),"(",paste0(sapply(x[-(1:2)],r2scala,showCode,symbolEnv),".toInt - 1",collapse=","),")")
    else if ( strings[1] == "<-" ) {
      prefix <- if ( ( typeof[2] == "symbol" ) && ( ! exists(strings[2],envir=symbolEnv) ) ) {
        assign(strings[2],"TRUE",envir=symbolEnv)
        "var "
      } else NULL
      paste0(prefix,r2scala(x[[2]],showCode,symbolEnv)," = ",r2scala(x[[3]],showCode,symbolEnv))
    }
    else if ( strings[1] == ":" ) paste0('_range(',r2scala(x[[2]],showCode,symbolEnv),',',r2scala(x[[3]],showCode,symbolEnv),')')
    else if ( strings[1] == "(" ) paste0("(",r2scala(x[[2]],showCode,symbolEnv),")")
    else if ( strings[1] == "+" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode,symbolEnv)," + ",r2scala(x[[3]],showCode,symbolEnv)) else paste0("+",r2scala(x[[2]],showCode,symbolEnv))
    else if ( strings[1] == "-" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode,symbolEnv)," - ",r2scala(x[[3]],showCode,symbolEnv)) else paste0("-",r2scala(x[[2]],showCode,symbolEnv))
    else if ( strings[1] == "*" ) paste0(r2scala(x[[2]],showCode,symbolEnv)," * ",r2scala(x[[3]],showCode,symbolEnv))
    else if ( strings[1] == "/" ) paste0(r2scala(x[[2]],showCode,symbolEnv),".toDouble / ",r2scala(x[[3]],showCode,symbolEnv))
    else if ( strings[1] == "%%" ) paste0(r2scala(x[[2]],showCode,symbolEnv)," % ",r2scala(x[[3]],showCode,symbolEnv))
    else if ( strings[1] == "^" ) paste0(r2scalaSubs("pow"),"(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( ( strings[1] == "I" ) && ( length(x) == 2 ) && ( typeof[2] == "character" ) ) paste0(strings[2])
    else if ( ( grepl("^eval[IDLRS][012]$",strings[1]) ) && ( typeof[2] == "character" ) ) {
      args <- if ( length(x) > 2 ) paste0(',',paste0(sapply(x[-(1:2)],r2scala,showCode,symbolEnv),collapse=",")) else NULL
      paste0('R.',strings[1],'("',strings[2],'"',args,')')
    }
    else if ( strings[1] == "if" ) {
      ifexpression = paste0('if ( ',r2scala(x[[2]],showCode,symbolEnv),' ) ',r2scala(x[[3]],showCode,symbolEnv))
      if ( length(x) == 4 ) paste0(ifexpression,' else ',r2scala(x[[4]],showCode,symbolEnv)) else ifexpression
    }
    else if ( strings[1] == "for" ) {
      paste0('for ( ',r2scala(x[[2]],showCode,symbolEnv),' <- ',r2scala(x[[3]],showCode,symbolEnv),' ) ',r2scala(x[[4]],showCode,symbolEnv))
    }
    else paste0(r2scalaSubs(strings[1]),"(",paste0(sapply(x[-1],r2scala,showCode,symbolEnv),collapse=","),")")
  }
  else if ( typeof[1] == "integer" ) paste0("{",strings[1],":Int}")
  else if ( typeof[1] == "double" ) paste0("{",strings[1],":Double}")
  else if ( typeof[1] == "logical" ) if (x[[1]]) "true" else "false"
  else if ( typeof[1] == "character" ) paste0('"""',strings[1],'"""')
  else stop("33")
}

r2scalaSubs <- function(x) {
  if ( x %in% c("abs","sqrt","log","log10","exp","pow","c","length","sum","mean","var","sd","max","min","cat","paste","paste0","nchar","seq","ceiling","floor","round","runif") ) paste0("_",x)
  else if ( x %in% c("as.numeric","as.double","as.integer","as.logical","as.character") ) paste0("_",gsub("\\.","DOT",x))
  else x
}
