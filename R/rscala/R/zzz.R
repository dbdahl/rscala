if ( identical(Sys.getenv("R_INSTALL_PKG"),"rscala") ) {
  cat("I'm installing the source package.\n")
  print(getwd())
  load(file.path("R","sysdata.rda"))
  scalaConfig()
} else {
  cat("No, I'm skipping an opportunity.\n")
}

