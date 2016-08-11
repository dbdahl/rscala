rscala: Bi-Directional Interface Between R and Scala with Callbacks
===================================================================

By David B. Dahl 
----------------

The rscala package provides a two-way bridge between R and Scala enabling a
user to exploit each language's respective strengths in a single project. The
rscala package transparently brings Scala and Java methods into R as if they
were native R functions. Specifically, Scala classes can be instantiated and
methods of Scala classes can be called directly. Furthermore, arbitrary Scala
code can be executed on the fly from within R, inline Scala functions can be
defined, and callbacks to the original R interpreter are supported. Finally,
rscala also enables arbitrary R code to be embedded within a Scala application.
The rscala package is available on CRAN and requires no special installations
or configurations of R or Scala. 

Install the package in R by executing:

```R
install.packages("rscala") 
```

Note that if only want to embed R in a Scala application, you don't need to
install the package. Simply add the following line to the your SBT build.sbt
file:

```scala
libraryDependencies += "org.ddahl" % "rscala_2.11" % "1.0.13" 
```

Paper:

* [Paper submitted to Journal of Statistical Software](https://dahl.byu.edu/software/rscala/rscala-jss.pdf). Note the paper pertains to version 1.0.6 of the package.
* [Code for all examples](https://dahl.byu.edu/software/rscala/replication-code.tar.gz) in paper.
* [Temperatures webapp](https://dahl.byu.edu/software/rscala/temperature/) from Section 5.2 from the paper.

Resources:

* [Git repository](https://dahl-git.byu.edu/dahl/rscala) containing source code and build & test scripts.
* [Package](https://cran.r-project.org/package=rscala) on [Comprehensive R Archive Network (CRAN)](http://cran.r-project.org/).
* [Scaladoc](https://dahl.byu.edu/software/rscala/scaladoc/org/ddahl/rscala/RClient.html) for embedding R in Scala.
* [Javadoc](https://dahl.byu.edu/software/rscala/javadoc/org/ddahl/rscala/RClient4Java.html) for embedding R in Java.

