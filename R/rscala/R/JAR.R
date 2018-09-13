# Although not exported, the rscala paper in JSS mentions this function.
JAR <- function(majorVersion) {
  list.files(file.path(find.package("rscala"),"java",paste0("scala-",majorVersion)),full.names=TRUE)
}
