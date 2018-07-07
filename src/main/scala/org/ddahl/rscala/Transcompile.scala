package org.ddahl.rscala

import scala.reflect.ClassTag

object Transcompile {

  val pi: Double = math.Pi
  val T: Boolean = true
  val F: Boolean = false

  def _numeric(n: Double): Array[Double] = _double(n)
  def _double(n: Double): Array[Double] = new Array[Double](n.toInt)
  def _integer(n: Double): Array[Int] = new Array[Int](n.toInt)
  def _logical(n: Double): Array[Boolean] = new Array[Boolean](n.toInt)
  def _character(n: Double): Array[String] = new Array[String](n.toInt)

  def _asDOTnumeric(x: Int): Double = _asDOTdouble(x)
  def _asDOTnumeric(x: Double): Double = _asDOTdouble(x)
  def _asDOTnumeric(x: Boolean): Double = _asDOTdouble(x)
  def _asDOTnumeric(x: String): Double = _asDOTdouble(x)

  def _asDOTnumeric(x: Array[Int]): Array[Double] = _asDOTdouble(x)
  def _asDOTnumeric(x: Array[Double]): Array[Double] = _asDOTdouble(x)
  def _asDOTnumeric(x: Array[Boolean]): Array[Double] = _asDOTdouble(x)
  def _asDOTnumeric(x: Array[String]): Array[Double] = _asDOTdouble(x)

  def _asDOTdouble(x: Int): Double = x.toDouble
  def _asDOTdouble(x: Double): Double = x
  def _asDOTdouble(x: Boolean): Double = if ( x ) 1.0 else 0.0
  def _asDOTdouble(x: String): Double = x.toDouble

  def _asDOTdouble(x: Array[Int]): Array[Double] = x.map(_.toDouble)
  def _asDOTdouble(x: Array[Double]): Array[Double] = x.clone
  def _asDOTdouble(x: Array[Boolean]): Array[Double] = x.map( if ( _ ) 1.0 else 0.0 )
  def _asDOTdouble(x: Array[String]): Array[Double] = x.map(_.toDouble)

  def _asDOTinteger(x: Int): Int = x
  def _asDOTinteger(x: Double): Int = x.toInt
  def _asDOTinteger(x: Boolean): Int = if ( x ) 1 else 0
  def _asDOTinteger(x: String): Int = x.toInt

  def _asDOTinteger(x: Array[Int]): Array[Int] = x.clone
  def _asDOTinteger(x: Array[Double]): Array[Int] = x.map(_.toInt)
  def _asDOTinteger(x: Array[Boolean]): Array[Int] = x.map( if ( _ ) 1 else 0 )
  def _asDOTinteger(x: Array[String]): Array[Int] = x.map(_.toInt)

  def _asDOTlogical(x: Int): Boolean = { x != 0 }
  def _asDOTlogical(x: Double): Boolean = { x != 0.0 }
  def _asDOTlogical(x: Boolean): Boolean = x
  def _asDOTlogical(x: String): Boolean = x.toBoolean

  def _asDOTlogical(x: Array[Int]): Array[Boolean] = x.map { _ != 0 }
  def _asDOTlogical(x: Array[Double]): Array[Boolean] = x.map { _ != 0.0 }
  def _asDOTlogical(x: Array[Boolean]): Array[Boolean] = x.clone
  def _asDOTlogical(x: Array[String]): Array[Boolean] = x.map { _.toBoolean }

  def _asDOTcharacter(x: Int): String = x.toString
  def _asDOTcharacter(x: Double): String = x.toString
  def _asDOTcharacter(x: Boolean): String = x.toString
  def _asDOTcharacter(x: String): String = x

  def _asDOTcharacter(x: Array[Int]): Array[String] = x.map { _.toString }
  def _asDOTcharacter(x: Array[Double]): Array[String] = x.map { _.toString }
  def _asDOTcharacter(x: Array[Boolean]): Array[String] = x.map { _.toString }
  def _asDOTcharacter(x: Array[String]): Array[String] = x.clone

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
  def _runif(n: Double): Array[Double] = Array.fill(n.toInt) { scala.util.Random.nextDouble() }

}

