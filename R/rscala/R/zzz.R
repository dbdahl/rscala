## Disable the potential installation of Scala during installation.
##
#if ( identical(Sys.getenv("R_INSTALL_PKG"),"rscala") ) {
# load(file.path("R","sysdata.rda"))
# scalaConfig()
#}

