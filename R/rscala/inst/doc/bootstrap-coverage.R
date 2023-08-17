makeConfidenceInterval <- function(p, n) {
  me <- qnorm(0.975) * sqrt( p * ( 1 - p ) / n )
  c(estimate = p, lower = p - me, upper = p + me)
}

p1    <- 0.75
p2    <- 0.35
truth <- qnorm(p1) / qnorm(p2)
n     <- 100
alpha <- 0.05



cat("######## rscala implementation #1")

library(rscala)
s <- scala()

coverage.rscala1 <- function(f, n, truth, p1, p2, nSamples, a, nIntervals) {
  coverage <- s(n = as.integer(n[1]), truth = as.double(truth[1]),
                p1 = as.double(p1[1]), p2 = as.double(p2[1]),
                nSamples = as.integer(nSamples[1]), a = as.double(a[1]),
                nIntervals = as.integer(nIntervals[1])) * '
    import scala.util.Random
    import scala.concurrent.{Await, Future, duration}
    import scala.concurrent.ExecutionContext.Implicits.global

    def quantile(sorted: Array[Double], p: Double) = {
      val i = ((sorted.length - 1) * p).asInstanceOf[Int]
      val delta = (sorted.length-1) * p - i
      ( 1 - delta ) * sorted(i) + delta * sorted( i + 1 )
    }

    def statistic(x: Array[Double]) = {
      scala.util.Sorting.quickSort(x)
      quantile(x, p1) / quantile(x, p2)
    }

    def resample(x: Array[Double], rng: Random) = Array.fill(x.length) {
      x(rng.nextInt(x.length))
    }

    def ciContains(x: Array[Double], rng: Random) = {
      val bs = Array.fill(nSamples) { statistic(resample(x, rng)) }
      scala.util.Sorting.quickSort(bs)
      quantile(bs, a / 2) <= truth  &&  truth <= quantile(bs, 1 - a / 2)
    }

    Await.result( Future.sequence( List.fill(nIntervals) {
      val dataset = R.evalD1("f(%-)", n)
      val seed = R.evalI0("sample(c(-1, 1), 1) * sample.int(2 ^ 31 - 1, 1)")
      val r = new Random(seed)
      Future { ciContains(dataset, r) }
    }), duration.Duration.Inf).count(identity) / nIntervals.toDouble
  '
  makeConfidenceInterval(coverage, nIntervals)
}



cat("######## All the remaining implementation use the parallel package.")

library(parallel)
cluster <- makeCluster(detectCores())



cat("######## rscala implementation #2")

clusterEvalQ(cluster, {
  library(rscala)
  s <- scala()
  ciContains.rscala2 <- function(f, n, truth, p1, p2, nSamples, a) {
    s(n = as.integer(n[1]), truth = as.double(truth[1]),
      p1 = as.double(p1[1]), p2 = as.double(p2[1]),
      nSamples = as.integer(nSamples[1]), a = as.double(a[1])) * '
      import scala.util.Random

      def quantile(sorted: Array[Double], p: Double) = {
        val i = (( sorted.length - 1 ) * p).asInstanceOf[Int]
        val delta = ( sorted.length - 1 ) * p - i
        ( 1 - delta ) * sorted(i) + delta * sorted( i + 1 )
      }

      def statistic(x: Array[Double]) = {
        scala.util.Sorting.quickSort(x)
        quantile(x, p1) / quantile(x, p2)
      }

      def resample(x: Array[Double], rng: Random) = Array.fill(x.length) {
        x(rng.nextInt(x.length))
      }

      val x = R.evalD1("f(%-)", n)
      val seed = R.evalI0("sample(c(-1, 1), 1) * sample.int(2 ^ 31 - 1, 1)")
      val r = new Random(seed)
      val bs = Array.fill(nSamples) { statistic(resample(x, r)) }
      scala.util.Sorting.quickSort(bs)
      quantile(bs, a / 2) <= truth  &&  truth <= quantile(bs, 1 - a / 2)
    '
  }
})

coverage.rscala2 <- function(f, n, truth, p1, p2, nSamples, a, nIntervals) {
  clusterExport(cluster, c("f", "n", "truth", "p1", "p2", "nSamples", "a"),
    envir = environment())
  coverage <- mean(parSapply(cluster, 1:nIntervals, function(i) {
    ciContains.rscala2(f, n, truth, p1, p2, nSamples, a)
  }))
  makeConfidenceInterval(coverage, nIntervals)
}



cat("######## Pure R implementation")

coverage.pureR <- function(f, n, truth, p1, p2, nSamples, a, nIntervals) {
  statistic <- function(x) {
    q <- quantile(x, probs = c(p1, p2))
    q[1] / q[2]
  }
  ciContains.pureR <- function(x) {
    samples <- numeric(nSamples)
    for ( i in seq_along(samples) ) {
      samples[i] <- statistic(sample(x, replace = TRUE))
    }
    ci <- quantile(samples, probs = c(a / 2, 1 - a / 2))
    ( ci[1] <= truth ) && ( truth <= ci[2] )
  }
  clusterExport(cluster, c("f", "n", "truth", "p1", "p2", "nSamples", "a"),
    envir = environment())
  coverage <- mean(parSapply(cluster, 1:nIntervals, function(i) {
    ciContains.pureR(f(n))
  }))
  makeConfidenceInterval(coverage, nIntervals)
}



cat("######## Rcpp implementation")

clusterEvalQ(cluster, {
  library(Rcpp)
  sourceCpp(code = "
    #include <Rcpp.h>
    using namespace Rcpp;

    double quantile(double *sorted, int length, double p) {
      int i = (int) (( length - 1 ) * p);
      double delta = ( length - 1 ) * p - i;
      return ( 1 - delta ) * sorted[i] + delta * sorted[ i + 1 ];
    }

    int compare_double(const void* a, const void* b) {
      double aa = *(double*) a;
      double bb = *(double*) b;
      if ( aa == bb ) return 0;
      return aa < bb ? -1 : 1;
    }

    double statistic(double *x, int length, double p1, double p2) {
      qsort(x, length, sizeof(double), compare_double);
      return quantile(x, length, p1) / quantile(x, length, p2);
    }

    double *resample(double *x, int length) {
      double *y = (double*) malloc( length * sizeof(double) );
      for ( int i = 0;  i < length; i++ ) {
        y[i] = x[ (int) (Rf_runif(0, 1) * length) ];
      }
      return y;
    }

    // [[Rcpp::export]]
    bool ciContains(NumericVector data, double truth,
                    double p1, double p2, int nSamples, double a) {
      double *y = (double*) malloc( nSamples * sizeof(double) );
      for ( int i = 0; i < nSamples; i++ ) {
        int length = data.size();
        double *z = resample(data.begin(), length);
        y[i] = statistic(z, length, p1, p2);
        free(z);
      }
      qsort(y, nSamples, sizeof(double), compare_double);
      bool result =  ( quantile(y, nSamples,     a / 2) <= truth ) &&
                     ( quantile(y, nSamples, 1 - a / 2) >= truth );
      free(y);
      return result;
    }
  ")
})

coverage.Rcpp <- function(f, n, truth, p1, p2, nSamples, a, nIntervals) {
  clusterExport(cluster, c("f", "n", "truth", "p1", "p2", "nSamples", "a"),
    envir = environment())
  coverage <- mean(parSapply(cluster, 1:nIntervals, function(i) {
    ciContains(f(n), truth, p1, p2, nSamples, a)
  }))
  makeConfidenceInterval(coverage, nIntervals)
}



cat("######## Benchmarks")

system2("hostname")
sessionInfo()
library(microbenchmark)
engine <- function(nSamples, nIntervals, times) microbenchmark(
  pureR   = coverage.pureR(
              rnorm, n, truth, p1, p2, nSamples, alpha, nIntervals),
  Rcpp    = coverage.Rcpp(
              rnorm, n, truth, p1, p2, nSamples, alpha, nIntervals),
  rscala1 = coverage.rscala1(
              rnorm, n, truth, p1, p2, nSamples, alpha, nIntervals),
  rscala2 = coverage.rscala2(
              rnorm, n, truth, p1, p2, nSamples, alpha, nIntervals),
  times = times)

engine(nSamples = 10000L, nIntervals = 10000L, times = 10)
