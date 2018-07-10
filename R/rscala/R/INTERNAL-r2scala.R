r2scala <- function(x, showCode, symbolEnv) {
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
      if ( strings[1] %in% c("yield","with","var","val","type","try","true","trait","throw","this","super","sealed","protected","private","package",
                             "override","object","null","new","match","lazy","import","implicit","forSome","finally","final","false","extends","do",
                             "def","class","catch","case","abstract") ) {
        paste0("_",strings[1])
      } else strings[1]
    }
    else if ( strings[1] == "return" ) {
      if ( length(x) != 2 ) stop('Too many arguments for return statement.') 
      paste0("return ",r2scala(x[[2]],showCode,symbolEnv))
    }
    else if ( strings[1] == "returnType" ) {
      if ( length(x) != 2 ) stop('returnType statement only take one argument.')
      assign("_returnType",eval(x[[2]]),envir=symbolEnv)
      ""
    }
    else if ( strings[1] == "break" ) "Outer.break"
    else if ( strings[1] == "next" ) "Inner.break"
    else if ( strings[1] == "{" )  paste0("{\n",paste0(sapply(x[-1],r2scala,showCode,symbolEnv),collapse="\n"),"\n}")
    else if ( strings[1] == "[" )  paste0(r2scala(x[[2]],showCode,symbolEnv),"(_ensureArray(",r2scala(x[[3]],showCode,symbolEnv),"))")
    else if ( strings[1] == "^" )  paste0(r2scalaSubs("pow"),   "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( strings[1] == "==" ) paste0(r2scalaSubs("equal"), "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( strings[1] == "<" )  paste0(r2scalaSubs("lt"),    "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( strings[1] == "<=" ) paste0(r2scalaSubs("le"),    "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( strings[1] == ">" )  paste0(r2scalaSubs("gt"),    "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
    else if ( strings[1] == ">=" ) paste0(r2scalaSubs("ge"),    "(",r2scala(x[[2]],showCode,symbolEnv),",",r2scala(x[[3]],showCode,symbolEnv),")")
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
    else if ( strings[1] == "/" ) paste0(r2scalaSubs("as.numeric"),"(",r2scala(x[[2]],showCode,symbolEnv),") / ",r2scala(x[[3]],showCode,symbolEnv))
    else if ( strings[1] == "%%" ) paste0(r2scala(x[[2]],showCode,symbolEnv)," % ",r2scala(x[[3]],showCode,symbolEnv))
    else if ( strings[1] == "c" ) paste0('_c(',paste0('_ensureArray(',sapply(x[-1],r2scala,showCode,symbolEnv),')',collapse=","),')')
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
      paste0('val Outer = new Breaks; val Inner = new Breaks; Outer.breakable { for ( ',r2scala(x[[2]],showCode,symbolEnv),' <- ',r2scala(x[[3]],showCode,symbolEnv),' ) Inner.breakable{ ',r2scala(x[[4]],showCode,symbolEnv),' } }')
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
  if ( x %in% c("numeric","double","integer","logical","character","subset","which","equal","lt","le","gt","ge","abs","sqrt","log","log10","exp","pow",
                "c","length","all","any","sum","mean","var","sd","max","min","cat","paste","paste0","nchar","rep","seq","ceiling","floor","round",
                "runif","rnorm") ) paste0("_",x)
  else if ( x %in% c("as.numeric","as.double","as.integer","as.logical","as.character") ) paste0("_",gsub("\\.","DOT",x))
  else x
}
