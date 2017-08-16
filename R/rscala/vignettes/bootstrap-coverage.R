#### Code for Section 4. "Case Study: Simulation Study Accelerated with rscala"

makeConfidenceInterval <- function(p, n) {
  me <- qnorm(0.975) * sqrt( p * ( 1 - p ) / n )
  c(estimate = p, lower = p - me, upper = p + me)
}


#### rscala implementation

library(rscala)
s <- scala()

coverage.rscala <- function(sampler=NULL, n=0L, truth=0, prob1=0.0, prob2=0.0,
                            nSamples=1000L, alpha=0.05, nIntervals=1000L) {
  coverage <- s %!% '
    import scala.util.Random
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global

    def quantile(sorted: Array[Double], p: Double) = {
      val i = ((sorted.length-1)*p).asInstanceOf[Int]
      val delta = (sorted.length-1)*p - i
      ( 1 - delta ) * sorted(i) + delta * sorted(i+1)
    }

    def statistic(x: Array[Double]) = {
      scala.util.Sorting.quickSort(x)
      quantile(x,prob1) / quantile(x,prob2)
    }

    def resample(x: Array[Double], rng: Random) = Array.fill(x.length) {
      x(rng.nextInt(x.length))
    }

    def ciContains(x: Array[Double], rng: Random) = {
      val bs = Array.fill(nSamples) { statistic(resample(x, rng)) }
      scala.util.Sorting.quickSort(bs)
      ( quantile(bs, alpha/2) <= truth ) && ( truth <= quantile(bs, 1-alpha/2) )
    }

    Await.result( Future.sequence( List.fill(nIntervals) {
      val dataset = R.invokeD1(sampler, n)
      val rng = new Random(R.invokeI0("runif", 1, -Int.MaxValue, Int.MaxValue))
      Future { ciContains(dataset, rng) }
    }), concurrent.duration.Duration.Inf).count(identity) / nIntervals.toDouble
  '
  makeConfidenceInterval(coverage, nIntervals)
}


#### The parallel package is used by the next two implementations

library(parallel)
cluster <- makeCluster(detectCores())


#### Pure R implementation

coverage.pureR <- function(sampler, n, truth, prob1, prob2, nSamples, alpha, nIntervals) {
  statistic <- function(x) {
    q <- quantile(x, probs = c(prob1, prob2))
    q[1] / q[2]
  }
  ciContains.pureR <- function(x) {
    samples <- sapply(1:nSamples, function(i) {
      statistic(sample(x, replace=TRUE))
    })
    ci <- quantile(samples, probs = c(alpha/2, 1-alpha/2))
    ( ci[1] <= truth ) && ( truth <= ci[2] )
  }
  clusterExport(cluster, c("sampler","n","truth","prob1","prob2","nSamples","alpha"),
    envir = environment())
  coverage <- mean(parSapply(cluster, 1:nIntervals, function(i) {
    ciContains.pureR(sampler(n))
  }))
  makeConfidenceInterval(coverage, nIntervals)
}


#### Rcpp implementation

clusterEvalQ(cluster, { # Don't count compile timing when benchmarking Rcpp.
  library(Rcpp)
  sourceCpp(code="
    #include <Rcpp.h>
    using namespace Rcpp;

    double quantile(double *sorted, int length, double p) {
      int i = (int) ((length-1)*p);
      double delta = (length-1)*p - i;
      return ( 1 - delta ) * sorted[i] + delta * sorted[i+1];
    }

    int compare_double(const void* a, const void* b) {
      double aa = *(double*)a;
      double bb = *(double*)b;
      if ( aa == bb ) return 0;
      return aa < bb ? -1 : 1;
    }

    double statistic(double *x, int length, double prob1, double prob2) {
      qsort(x, length, sizeof(double), compare_double);
      return quantile(x, length, prob1) / quantile(x, length, prob2);
    }

    double *resample(double *x, int length) {
      double *y = (double*) malloc(length*sizeof(double));
      for ( int i=0; i<length; i++ ) y[i] = x[(int)(Rf_runif(0,1)*length)];
      return y;
    }

    // [[Rcpp::export]]
    bool ciContains(NumericVector data, double truth,
                    double prob1, double prob2, int nSamples, double alpha) {
      double *y = (double*) malloc(nSamples*sizeof(double));
      for ( int i=0; i<nSamples; i++ ) {
        int length = data.size();
        double *z = resample(data.begin(), length);
        y[i] = statistic(z, length, prob1, prob2);
        free(z);
      }
      qsort(y, nSamples, sizeof(double), compare_double);
      bool result =  ( quantile(y, nSamples,   alpha/2) <= truth ) &&
                     ( quantile(y, nSamples, 1-alpha/2) >= truth );
      free(y);
      return result;
    }
  ")
})

coverage.Rcpp <- function(sampler, n, truth, prob1, prob2, nSamples, alpha, nIntervals) {
  clusterExport(cluster, c("sampler","n","truth","prob1","prob2","nSamples","alpha"),
    envir=environment())
  coverage <- mean(parSapply(cluster, 1:nIntervals, function(i) {
    ciContains(sampler(n), truth, prob1, prob2, nSamples, alpha)
  }))
  makeConfidenceInterval(coverage, nIntervals)
}


#### Benchmarks

prob1   <- 0.75
prob2   <- 0.35
alpha   <- 0.05
n       <- 100
truth   <- qnorm(prob1) / qnorm(prob2)

library(microbenchmark)
engine <- function(nSamples, nIntervals) {
  microbenchmark(
    pureR.  = coverage.pureR( rnorm, n, truth, prob1, prob2, nSamples, alpha, nIntervals),
    Rcpp.   = coverage.Rcpp(  rnorm, n, truth, prob1, prob2, nSamples, alpha, nIntervals),
    rscala. = coverage.rscala(rnorm, n, truth, prob1, prob2, nSamples, alpha, nIntervals),
    times=30)
}

engine(nSamples = 1000L, nIntervals = 1000L)
