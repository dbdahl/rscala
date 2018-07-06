package org.ddahl.rscala

import scala.reflect.ClassTag

object Transcompile {

  val pi: Double = math.Pi

  def _abs(x: Double): Double = math.abs(x)
  def _abs(x: Int): Int = math.abs(x)

  def _sqrt(x: Double): Double = math.sqrt(x)
  def _log(x: Double): Double = math.log(x)
  def _log10(x: Double): Double = math.log10(x)
  def _exp(x: Double): Double = math.exp(x)
  def _pow(x: Double, y: Double): Double = math.pow(x,y)

  def _c[A: ClassTag](x: A*): Array[A] = x.toArray

  def _mean(x: Array[Double]): Double = x.sum / x.length
  def _mean(x: Array[Int]): Double = _mean(x.map(_.toDouble))

  def _variance(x: Array[Double]): Double = {
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
  def _variance(x: Array[Int]): Double = _variance(x.map(_.toDouble))

  def _sd(x: Array[Double]): Double = _sqrt(_variance(x))
  def _sd(x: Array[Int]): Double = _sqrt(_variance(x))

  def _max(x: Array[Double]): Double = x.max
  def _max(x: Array[Int]): Int = x.max

  def _min(x: Array[Double]): Double = x.min
  def _min(x: Array[Int]): Int = x.min

  def _cat(x: Object*): Unit = print(_paste(x:_*))
  def _paste(x: Object*): String = x.mkString(" ")
  def _paste0(x: Object*): String = x.mkString

  def _nchar(x: String): Int = x.length

}

