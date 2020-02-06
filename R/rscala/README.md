# rscala: Bridge Between 'R' and 'Scala' with Callbacks

#### By David B. Dahl (Brigham Young University)

The rscala package provides a bridge between R and Scala, enabling a user to
exploit each language's respective strengths in a single project. The rscala
package brings Scala and Java libraries into R with a simple interface.
Specifically, Scala classes can be instantiated and methods of Scala classes
can be called directly. Furthermore, arbitrary Scala code can be executed on
the fly from within R and callbacks to R are supported. Conversely, rscala also
enables arbitrary R code to be embedded within a Scala application.


## Installation

In R, install the package by executing:

```R
install.packages("rscala") 
```

To install --- or check the compatability of your existing installation of ---
Scala and Java, please execute:

```R
rscala::scalaConfig()
```

Note that if only want to embed R in a Scala application, you don't need to
install the package. Simply add the following line to the your SBT build.sbt
file:

```scala
libraryDependencies += "org.ddahl" %% "rscala" % "3.2.17"
```


## Usage guides

A paper describing the software is "conditionally accepted" and "in editing" at
the [Journal of Statistical Software](https://www.jstatsoft.org).  The latest
citation information is available using:

```R
citation("rscala")
```

An [updated version of the
paper](https://dahl.byu.edu/public/rscala/rscala.pdf)
is available
[here](https://dahl.byu.edu/public/rscala/rscala.pdf)
or as a vignette in R (once the package is installed):

```R
vignette("rscala")
```

The functionality of the software is also described and demonstrated in the
help files:

```R
library(help="rscala")
library(rscala)
example(scala)

```


## Example packages built with rscala

R extensions can be written using this package, as demonstrated by these
packages:

* [sdols](https://CRAN.R-project.org/package=sdols)
* [shallot](https://CRAN.R-project.org/package=shallot)
* [bamboo](https://CRAN.R-project.org/package=bamboo)


## Resources

* [Vignette](https://dahl.byu.edu/public/rscala/rscala.pdf) describing the package usage.
* [Git repository](https://github.com/dbdahl/rscala) containing source code and build & test scripts.
* [Scaladoc](https://dahl.byu.edu/public/rscala/doc/org/ddahl/rscala/RClient.html) for RClient class to access R from Scala.
* [Package on CRAN](https://CRAN.R-project.org/package=rscala).

