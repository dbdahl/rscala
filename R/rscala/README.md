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
install.packages("remotes")
library("remotes")
remotes::install_github("dbdahl/rscala/R/rscala")
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
libraryDependencies += "org.ddahl" %% "rscala" % "3.2.19"
```
Or if you're managing dependencies with Maven, you may add the following to
 your `pom.xml` file, replacing the version numbers for Scala and for this 
library as appropriate.
```xml
<dependency>
    <groupId>org.ddahl</groupId>
    <artifactId>rscala_2.13</artifactId>
    <version>3.2.19</version>
</dependency>
```

## Usage guides

The original paper describing the software is:

D. B. Dahl (2020), Integration of R and Scala using rscala, [**Journal of Statistical Software**](https://www.jstatsoft.org), 92:4, 1-18, *doi:10.18637/jss.v092.i04*.

The citation information is available using:

```R
citation("rscala")
```

An [updated version of the paper](https://github.com/dbdahl/rscala/blob/master/R/rscala/inst/doc/rscala.pdf?raw=true)
is available [here](https://github.com/cran/rscala/blob/master/inst/doc/rscala.pdf?raw=true).


## Example packages built with rscala

R extensions can be written using this package, as demonstrated by these
packages:

* [aibd](https://github.com/dbdahl/aibd)
* [shallot](https://github.com/dbdahl/shallot)
* [bamboo](https://github.com/dbdahl/bamboo)


## Resources

* [Vignette](https://github.com/cran/rscala/blob/master/inst/doc/rscala.pdf?raw=true) describing the package usage.
* [Git repository](https://github.com/dbdahl/rscala) containing source code and build & test scripts.
* [Scaladoc](https://dahl.byu.edu/rscala/org/ddahl/rscala/RClient.html) for RClient class to access R from Scala.

