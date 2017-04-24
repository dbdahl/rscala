lEtTeRs <- c(letters,LETTERS)
alphabet <- c(lEtTeRs,0:9)

uniqueName <- function(value, workspace, prefix, inherits=TRUE, length=8) {
  while (TRUE) {
    # name <- sprintf("%s%d",prefix,sample.int(.Machine$integer.max,1L))
    name <- paste0(prefix,sample(lEtTeRs,1),paste0(sample(alphabet,length-1,replace=TRUE),collapse=""))
    if ( ! exists(name,envir=workspace,inherits=inherits) ) {
      assign(name,value,envir=workspace,inherits=FALSE)
      return(name)
    }
  }
}

