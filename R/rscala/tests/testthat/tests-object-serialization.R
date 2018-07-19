context("object-serialization")

# skip("object-serialization")

myExpect <- function(obj,name=NULL,identical=TRUE,tolerance=sqrt(.Machine$double.eps)) {
  ref <- scalaSerialize(obj,s,name=name)
  newObj <- scalaUnserialize(ref)
  if ( identical ) expect_identical(newObj, obj)
  else expect_equal(newObj, obj, tolerance=tolerance)
}

test_that("Basic serialization works", {
  myExpect(list(a=1, b=c(TRUE,FALSE), c=3.0))
  myExpect(mtcars,name="MTCars")
  iris$Species <- as.character(iris$Species)
  names(iris) <- gsub("\\W","_",names(iris))
  myExpect(iris)
})
