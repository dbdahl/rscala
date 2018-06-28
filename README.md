rscala: Bridge Between 'R' and 'Scala' with Callbacks
===================================================================

By David B. Dahl, Brigham Young University
------------------------------------------

The rscala package provides a bridge between R and Scala, enabling a user to
exploit each language's respective strengths in a single project. The rscala
package brings Scala and Java libraries into R with a simple interface.
Specifically, Scala classes can be instantiated and methods of Scala classes
can be called directly. Furthermore, arbitrary Scala code can be executed on
the fly from within R and callbacks to R are supported.  The rscala package is
available on CRAN and requires no special installations or configurations of R
or Scala. 

Install the package in R by executing:

```R
install.packages("rscala") 
```

A paper describing an older version of the software is awaiting peer review at
the [Journal of Statistical Softwware](https://www.jstatsoft.org).  The paper
will be updated to the current version when the peer review is available.  In
the meantime, the functionality of the paper is described and demonstrated
in the help files:

```R
library(help="rscala")
library(rscala)
example(scala)

```

R extensions can be written using this package, as demonstrated by these packages:

* [sdols](https://CRAN.R-project.org/package=sdols)
* [shallot](https://CRAN.R-project.org/package=shallot)
* [bamboo](https://CRAN.R-project.org/package=bamboo)

Resources:

* [Git repository](https://github.com/dbdahl/rscala) containing source code and build & test scripts.
* [Package](https://CRAN.R-project.org/package=rscala) on [Comprehensive R Archive Network (CRAN)](HTTP://CRAN.r-project.org/).

