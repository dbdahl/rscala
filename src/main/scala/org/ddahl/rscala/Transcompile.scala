package org.ddahl.rscala

import scala.reflect.ClassTag

object Transcompile {

  val pi: Double = math.Pi
  val T: Boolean = true
  val F: Boolean = false

  def _abs(x: Double): Double = math.abs(x)
  def _abs(x: Int): Int = math.abs(x)
  def _sqrt(x: Double): Double = math.sqrt(x)
  def _log(x: Double): Double = math.log(x)
  def _log10(x: Double): Double = math.log10(x)
  def _exp(x: Double): Double = math.exp(x)
  def _pow(x: Double, y: Double): Double = math.pow(x,y)

  def _c[A: ClassTag](x: A*): Array[A] = x.toArray

  def _length[A](x: Array[A]): Int = x.length

  def _sum(x: Array[Double]): Double = x.sum
  def _sum(x: Array[Int]): Int = x.sum
  def _sum(x: Array[Boolean]): Int = x.map(if ( _ ) 1 else 0).sum

  def _mean(x: Array[Double]): Double = x.sum / x.length
  def _mean(x: Array[Int]): Double = _mean(x.map(_.toDouble))
  def _mean(x: Array[Boolean]): Double = _mean(x.map(if ( _ ) 1.0 else 0.0))

  def _var(x: Array[Double]): Double = {
    val n = x.length
    if ( n < 2 ) 0.0
    else {
      val K = x(0)
      var ex = 0.0
      var ex2 = 0.0
      x foreach { y =>
        val d = y - K
        ex += d
        ex2 += d * d
      }
      (ex2 - (ex * ex) / n) / (n - 1)
    }
  }
  def _var(x: Array[Int]): Double = _var(x.map(_.toDouble))
  def _var(x: Array[Boolean]): Double = _var(x.map(if ( _ ) 1.0 else 0.0))

  def _sd(x: Array[Double]): Double = _sqrt(_var(x))
  def _sd(x: Array[Int]): Double = _sqrt(_var(x))
  def _sd(x: Array[Boolean]): Double = _sqrt(_var(x))

  def _max(x: Array[Double]): Double = x.max
  def _max(x: Array[Int]): Int = x.max

  def _min(x: Array[Double]): Double = x.min
  def _min(x: Array[Int]): Int = x.min

  def _cat(x: Any*): Unit = print(_paste(x:_*))
  def _paste(x: Any*): String = x.mkString(" ")
  def _paste0(x: Any*): String = x.mkString

  def _nchar(x: String): Int = x.length

  def _range(lower: Double, upper: Double): Array[Int] = Array.range(lower.toInt, upper.toInt + 1)

  def _seq(from: Double, to: Double, length: Int): Array[Double] = {
    val by = ( to - from ) / ( length - 1 )
    Array.tabulate(length) { i =>
      from + i*by
    }
  }

  def _seq(from: Double, to: Double, by: Double): Array[Double] = {
    val length = ( ( to - from ) / by + 1 ).floor.toInt
    Array.tabulate(length) { i =>
      from + i*by
    }
  }

  def _ceiling(x: Double): Double = math.ceil(x)
  def _floor(x: Double): Double = math.floor(x)
  def _round(x: Double): Double = math.round(x)

  def _runif(): Double = scala.util.Random.nextDouble()
  def _runif(n: Int): Array[Double] = Array.fill(n) { scala.util.Random.nextDouble() }

}

