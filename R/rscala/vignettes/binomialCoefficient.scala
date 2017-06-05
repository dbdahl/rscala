def c(n: Int, k: Int) = (1 to k).map( i => (n+1-i)/i ).product
c(10,3)

def k(n: Int, k: Int) = (1 to n).product / ( (1 to k).product * (1 to (n-k)).product )


k(10,3)
k(10,9)



object BinomialCoefficient {

  def apply(n: Int, k: Int): BigInt = {
    val cache = Array.ofDim[BigInt](n+1,k+1)
    for ( i <- cache.indices ) {
      for ( j <- 0 to math.min(i,k) ) {
        cache(i)(j) = if ( ( j == 0 ) || ( j == i ) ) 1
        else cache(i-1)(j-1) + cache(i-1)(j)
      }
    }
    cache(n)(k)
  }

}

BinomialCoefficient(10,0)
BinomialCoefficient(10,3)
BinomialCoefficient(10,7)

BinomialCoefficient(100,70)


object BinomialCoefficient2 {

  def apply(n: Int, k: Int): BigInt = {
    val cache = Array.fill(k+1)(BigInt(0))
    cache(0) = 1
    for ( i <- 1 to n ) {
      for ( j <- math.min(i,k) until 0 by -1 ) {
        cache(j) = cache(j) + cache(j-1)
      }
    }
    cache(k)
  }

}

BinomialCoefficient2(10,0)
BinomialCoefficient2(10,3)
BinomialCoefficient2(10,7)

BinomialCoefficient2(100,70)
BinomialCoefficient2(1000,400)


object BinomialCoefficient3 {

  def apply(n: Int, k: Int) = {
    if ( ( n < k ) || ( k < 0 ) ) BigInt(0)
    else {
      val kk = if ( k > n/2 ) n-k else k
      (1 to kk).foldLeft(BigInt(1)) { (product,i) => product * (n-i+1)/i }
    }
  }

}

BinomialCoefficient3(10000,300)

