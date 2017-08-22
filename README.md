rscala: Bi-Directional Interface Between R and Scala with Callbacks
===================================================================

By David B. Dahl, Brigham Young University
------------------------------------------

The rscala package provides a two-way bridge between R and Scala enabling a
user to exploit each language's respective strengths in a single project. The
rscala package brings Scala and Java libraries into R with a simple interface.
Specifically, Scala classes can be instantiated and methods of Scala classes
can be called directly. Furthermore, arbitrary Scala code can be executed on
the fly from within R, inline Scala functions can be defined, and callbacks to
the original R interpreter are supported. Finally, rscala also enables
arbitrary R code to be embedded within a Scala or Java application.  The rscala
package is available on CRAN and requires no special installations or
configurations of R or Scala. 

Install the package in R by executing:

```R
install.packages("rscala") 
```

Note that if only want to embed R in a Scala application, you don't need to
install the package. Simply add the following line to the your SBT build.sbt
file:

```scala
libraryDependencies += "org.ddahl" %% "rscala" % "2.3.1"
```

Paper:

* [Paper submitted to Journal of Statistical Software](https://dahl.byu.edu/software/rscala/rscala-jss.pdf). Note the paper pertains to version 2.3.1 of the package.
* [Temperatures webapp](https://dahl.byu.edu/software/rscala/temperature/) from Section 3 from the paper.

Resources:

* [Git repository](https://github.com/dbdahl/rscala) containing source code and build & test scripts.
* [Package](https://cran.r-project.org/package=rscala) on [Comprehensive R Archive Network (CRAN)](http://cran.r-project.org/).
* [Scaladoc](https://dahl.byu.edu/software/rscala/scaladoc/org/ddahl/rscala/RClient.html) for embedding R in Scala.

