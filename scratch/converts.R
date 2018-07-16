library(rscala)
scala()

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
scalaDeclareList(s, myList, "MyList", andConvert=FALSE)
a <- scalaSerializeList(s, myList, "MyList")
a$a()
identical(scalaUnserializeList(a),myList)

iris$Species <- as.character(iris$Species)
scalaDeclareList(s, iris, "Iris", andConvert=FALSE)
a <- scalaSerializeList(s, iris, "Iris")
a$isDataFrame()
a$Sepal_Length()
identical(scalaUnserializeList(a),iris)


a <- scalaSerializeList(s, mtcars, "MTCars")
identical(scalaUnserializeList(a),mtcars)



big <- rbind(iris)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
a <- scalaSerializeList(s, big, "Iris")
identical(scalaUnserializeList(a),big)



