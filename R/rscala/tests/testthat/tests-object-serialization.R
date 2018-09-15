context("object-serialization")

# skip("object-serialization")

myExpect <- function(obj,method,...) {
  ref <- scalaPush(obj,method,s,...)
  newObj <- scalaPull(ref,method)
  expect_identical(newObj, obj)
}

test_that("Basic serialization works", {
  myExpect(list(a=1, b=c(TRUE,FALSE), c=I(3.0)), "generic", as.is=TRUE)
  myExpect(mtcars,"list")
  iris$Species <- as.character(iris$Species)
  names(iris) <- gsub("\\W","_",names(iris))
  myExpect(iris,"list")
})
