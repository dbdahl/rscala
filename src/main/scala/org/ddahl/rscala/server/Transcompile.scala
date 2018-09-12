package org.ddahl.rscala.server

import scala.reflect.ClassTag

object Transcompile extends TranscompileStub {

  import scala.Double.NaN
  import java.lang.Double.isNaN

  private def r2int(x: Double) = math.round(x).toInt
  private def f2int(x: Double) = math.floor(x).toInt
  private def c2int(x: Double) = math.ceil(x).toInt

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

  def _which(x: Array[Boolean]): Array[Int] = x.zipWithIndex.filter(_._1).map(_._2)

  def _equal(x: Double, y: Double): Boolean = x == y
  def _equal(x: Array[Double], y: Double): Array[Boolean] = x map { _ == y }
  def _equal(x: Array[Int], y: Double): Array[Boolean] = x map { _ == y }
  def _equal(x: Double, y: Array[Double]): Array[Boolean] = y map { x == _ }
  def _equal(x: Double, y: Array[Int]): Array[Boolean] = y map { x == _ }
  def _equal(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 == z._2 }
  def _equal(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 == z._2 }
  def _equal(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 == z._2 }
  def _equal(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 == z._2 }
  def _equal(x: Boolean, y: Boolean): Boolean = x == y
  def _equal(x: Array[Boolean], y: Boolean): Array[Boolean] = x map { _ == y }
  def _equal(x: Boolean, y: Array[Boolean]): Array[Boolean] = y map { x == _ }
  def _equal(x: Array[Boolean], y: Array[Boolean]): Array[Boolean] = x zip y map { z => z._1 == z._2 }
  def _equal(x: String, y: String): Boolean = x == y
  def _equal(x: Array[String], y: String): Array[Boolean] = x map { _ == y }
  def _equal(x: String, y: Array[String]): Array[Boolean] = y map { x == _ }
  def _equal(x: Array[String], y: Array[String]): Array[Boolean] = x zip y map { z => z._1 == z._2 }

  def _notequal(x: Double, y: Double): Boolean = x != y
  def _notequal(x: Array[Double], y: Double): Array[Boolean] = x map { _ != y }
  def _notequal(x: Array[Int], y: Double): Array[Boolean] = x map { _ != y }
  def _notequal(x: Double, y: Array[Double]): Array[Boolean] = y map { x != _ }
  def _notequal(x: Double, y: Array[Int]): Array[Boolean] = y map { x != _ }
  def _notequal(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 != z._2 }
  def _notequal(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 != z._2 }
  def _notequal(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 != z._2 }
  def _notequal(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 != z._2 }
  def _notequal(x: Boolean, y: Boolean): Boolean = x != y
  def _notequal(x: Array[Boolean], y: Boolean): Array[Boolean] = x map { _ != y }
  def _notequal(x: Boolean, y: Array[Boolean]): Array[Boolean] = y map { x != _ }
  def _notequal(x: Array[Boolean], y: Array[Boolean]): Array[Boolean] = x zip y map { z => z._1 != z._2 }
  def _notequal(x: String, y: String): Boolean = x != y
  def _notequal(x: Array[String], y: String): Array[Boolean] = x map { _ != y }
  def _notequal(x: String, y: Array[String]): Array[Boolean] = y map { x != _ }
  def _notequal(x: Array[String], y: Array[String]): Array[Boolean] = x zip y map { z => z._1 != z._2 }

  def _lt(x: Double, y: Double): Boolean = x < y
  def _lt(x: Array[Double], y: Double): Array[Boolean] = x map { _ < y }
  def _lt(x: Array[Int], y: Double): Array[Boolean] = x map { _ < y }
  def _lt(x: Double, y: Array[Double]): Array[Boolean] = y map { x < _ }
  def _lt(x: Double, y: Array[Int]): Array[Boolean] = y map { x < _ }
  def _lt(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 < z._2 }
  def _lt(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 < z._2 }
  def _lt(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 < z._2 }
  def _lt(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 < z._2 }

  def _le(x: Double, y: Double): Boolean = x <= y
  def _le(x: Array[Double], y: Double): Array[Boolean] = x map { _ <= y }
  def _le(x: Array[Int], y: Double): Array[Boolean] = x map { _ <= y }
  def _le(x: Double, y: Array[Double]): Array[Boolean] = y map { x <= _ }
  def _le(x: Double, y: Array[Int]): Array[Boolean] = y map { x <= _ }
  def _le(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 <= z._2 }
  def _le(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 <= z._2 }
  def _le(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 <= z._2 }
  def _le(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 <= z._2 }

  def _gt(x: Double, y: Double): Boolean = x > y
  def _gt(x: Array[Double], y: Double): Array[Boolean] = x map { _ > y }
  def _gt(x: Array[Int], y: Double): Array[Boolean] = x map { _ > y }
  def _gt(x: Double, y: Array[Double]): Array[Boolean] = y map { x > _ }
  def _gt(x: Double, y: Array[Int]): Array[Boolean] = y map { x > _ }
  def _gt(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 > z._2 }
  def _gt(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 > z._2 }
  def _gt(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 > z._2 }
  def _gt(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 > z._2 }

  def _ge(x: Double, y: Double): Boolean = x >= y
  def _ge(x: Array[Double], y: Double): Array[Boolean] = x map { _ >= y }
  def _ge(x: Array[Int], y: Double): Array[Boolean] = x map { _ >= y }
  def _ge(x: Double, y: Array[Double]): Array[Boolean] = y map { x >= _ }
  def _ge(x: Double, y: Array[Int]): Array[Boolean] = y map { x >= _ }
  def _ge(x: Array[Double], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 >= z._2 }
  def _ge(x: Array[Double], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 >= z._2 }
  def _ge(x: Array[Int], y: Array[Double]): Array[Boolean] = x zip y map { z => z._1 >= z._2 }
  def _ge(x: Array[Int], y: Array[Int]): Array[Boolean] = x zip y map { z => z._1 >= z._2 }

  def _and(x: Array[Boolean], y: Array[Boolean]): Array[Boolean] = x zip y map { z => z._1 & z._2 }
  def _and(x: Array[Boolean], y: Boolean): Array[Boolean] = x map { _ & y }
  def _and(x: Boolean, y: Array[Boolean]): Array[Boolean] = y map { x & _ }
  def _and(x: Boolean, y: Boolean): Boolean = x & y

  def _or(x: Array[Boolean], y: Array[Boolean]): Array[Boolean] = x zip y map { z => z._1 | z._2 }
  def _or(x: Array[Boolean], y: Boolean): Array[Boolean] = x map { _ | y }
  def _or(x: Boolean, y: Array[Boolean]): Array[Boolean] = y map { x | _ }
  def _or(x: Boolean, y: Boolean): Boolean = x | y

  def _abs(x: Int): Int = math.abs(x)
  def _abs(x: Double): Double = math.abs(x)
  def _abs(x: Array[Double]): Array[Double] = x map { math.abs }
  def _abs(x: Array[Int]): Array[Int] = x map { math.abs }

  def _sqrt(x: Double): Double = math.sqrt(x)
  def _sqrt(x: Array[Double]): Array[Double] = x map { math.sqrt }
  def _sqrt(x: Array[Int]): Array[Double] = x map { xx => math.sqrt(xx) }

  def _log(x: Double): Double = math.log(x)
  def _log(x: Array[Double]): Array[Double] = x map { math.log }
  def _log(x: Array[Int]): Array[Double] = x map { xx => math.log(xx) }

  def _log10(x: Double): Double = math.log10(x)
  def _log10(x: Array[Double]): Array[Double] = x map { math.log10 }
  def _log10(x: Array[Int]): Array[Double] = x map { xx => math.log10(xx) }

  def _exp(x: Double): Double = math.exp(x)
  def _exp(x: Array[Double]): Array[Double] = x map { math.exp }
  def _exp(x: Array[Int]): Array[Double] = x map { xx => math.exp(xx) }

  def _pow(x: Double, y: Double): Double = math.pow(x,y)
  def _pow(x: Double, y: Array[Double]): Array[Double] = y map { yy => math.pow(x,yy) }
  def _pow(x: Double, y: Array[Int]): Array[Double] = y map { yy => math.pow(x,yy) }
  def _pow(x: Array[Double], y: Double): Array[Double] = x map { xx => math.pow(xx,y) }
  def _pow(x: Array[Int], y: Double): Array[Double] = x map { xx => math.pow(xx,y) }
  def _pow(x: Array[Double], y: Array[Double]): Array[Double] = x zip y  map { zz => math.pow(zz._1,zz._2) }
  def _pow(x: Array[Int], y: Array[Int]): Array[Double] = x zip y map { zz => math.pow(zz._1,zz._2) }
  def _pow(x: Array[Double], y: Array[Int]): Array[Double] = x zip y map { zz => math.pow(zz._1,zz._2) }
  def _pow(x: Array[Int], y: Array[Double]): Array[Double] = x zip y map { zz => math.pow(zz._1,zz._2) }

  def _c(x: Array[Double], y: Array[Int]): Array[Double] = x ++ y.map(_.toDouble)
  def _c(x: Array[Int], y: Array[Double]): Array[Double] = x.map(_.toDouble) ++ y
  def _c(x: Array[Double], y: Array[String]): Array[String] = x.map(_.toString) ++ y
  def _c(x: Array[Int], y: Array[String]): Array[String] = x.map(_.toString) ++ y
  def _c(x: Array[String], y: Array[Int]): Array[String] = x ++ y.map(_.toString)
  def _c(x: Array[String], y: Array[Double]): Array[String] = x ++ y.map(_.toString)
  def _c(x: Array[Int]*): Array[Int] = x.toArray.flatten
  def _c(x: Array[Double]*): Array[Double] = x.toArray.flatten
  def _c(x: Array[Boolean]*): Array[Boolean] = x.toArray.flatten
  def _c(x: Array[String]*): Array[String] = x.toArray.flatten

  def _length[A](x: Array[A]): Int = x.length
  def _length(x: Double): Int = 1
  def _length(x: Int): Int = 1

  def _all(x: Array[Boolean]): Boolean = x.forall(identity)
  def _any(x: Array[Boolean]): Boolean = x.exists(identity)

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
  def _paste(x: Any*): String = {
    x.map { xx =>
      if ( xx.getClass.isArray ) xx.asInstanceOf[Array[_]].map(xxx => _paste0(xxx)).mkString(" ")
      else xx.toString
    }.mkString(" ")
  }
  def _paste0(x: Any*): String = {
    x.map { xx =>
      if ( xx.getClass.isArray ) xx.asInstanceOf[Array[_]].map(xxx => _paste0(xxx)).mkString
      else xx.toString
    }.mkString
  }

  def _nchar(x: String): Int = x.length

  def _rep(x: Double, times: Double): Array[Double] = {
    Array.fill(r2int(times))(x)
  }

  def _rep(x: Array[Double], times: Array[Double] = null, each: Double = NaN): Array[Double] = {
    val count = ( if ( times == null ) 0 else 1 ) + ( if ( isNaN(each) ) 0 else 1 )
    if ( count != 1 ) sys.error("Exactly one of 'times' and 'each' should be provided." )
    if ( times != null ) {
      x.zip(times).flatMap { y => Array.fill(f2int(y._2))(y._1) }
    } else {
      x.flatMap(y => Array.fill(f2int(each))(y))
    }
  }

  def _range(lower: Double, upper: Double): Array[Int] = Array.range(lower.toInt, upper.toInt + 1)

  def _seq(from: Double, to: Double, by: Double = NaN, length_out: Double = NaN, along_with: Array[_] = null): Array[Double] = {
    val count = ( if ( isNaN(by) ) 0 else 1 ) + ( if ( isNaN(length_out) ) 0 else 1 ) + ( if ( along_with == null ) 0 else 1 )
    if ( count != 1 ) sys.error("Exactly one of 'by', 'length.out', and 'along.with' should be provided." )
    val (by2,length2) = if ( ! isNaN(by) ) {
      (by, f2int( ( to - from ) / by + 1 ))
    } else if ( ! isNaN(length_out) ) {
      val len = c2int(length_out)
      (( to - from ) / ( len - 1 ), len)
    } else {
      val len = along_with.length
      (( to - from ) / ( len - 1 ), len)
    }
    Array.tabulate(length2) { from + _*by2 }
  }

  def _seq_along[A](alongWith: Array[A]): Array[Int] = Array.range(1, alongWith.length+1)
  def _seq_len[A](lengthOut: Double): Array[Int] = Array.range(1, f2int(lengthOut)+1)

  def _ceiling(x: Double): Double = math.ceil(x)
  def _ceiling(x: Array[Double]): Array[Double] = x map { math.ceil }

  def _floor(x: Double): Double = math.floor(x)
  def _floor(x: Array[Double]): Array[Double] = x map { math.floor }

  def _round(x: Double): Double = math.round(x)
  def _round(x: Array[Double]): Array[Double] = x map { z => math.round(z).toDouble }

  def _random(): Double = scala.util.Random.nextDouble()
  def _random(n: Double): Array[Double] = Array.fill(n.toInt) { scala.util.Random.nextDouble() }

  def _stop(msg: String = ""): Unit = throw new RuntimeException("Error: "+msg)
  def _warning(msg: String = ""): Unit = println("Warning message:\n"+msg)

  def _ensureArray(x: Int): Array[Int] = Array(x)
  def _ensureArray(x: Double): Array[Double] = Array(x)
  def _ensureArray(x: Boolean): Array[Boolean] = Array(x)
  def _ensureArray(x: String): Array[String] = Array(x)
  def _ensureArray(x: Array[Int]): Array[Int] = x
  def _ensureArray(x: Array[Double]): Array[Double] = x
  def _ensureArray(x: Array[Boolean]): Array[Boolean] = x
  def _ensureArray(x: Array[String]): Array[String] = x

  class Index private (val x: Int)
  object Index {
    def apply(x: Int) = new Index(x-1)
    def apply(x: Double) = new Index(x.toInt-1)
  }

  class Indices private (val x: Array[Int])
  object Indices {
    def apply(x: Array[Int]) = new Indices(x.map(_-1))
    def apply(x: Array[Double]) = new Indices(x.map(_.toInt-1))
    def apply(x: Array[Boolean]) = new Indices(x.zipWithIndex.filter(_._1).map(_._2))
    def apply(x: Boolean) = new Indices( if ( x ) Array[Int](1) else Array[Int]() )
  }

  def _mkIndex(x: Int) = Index(x)
  def _mkIndex(x: Double) = Index(x)
  def _mkIndex(x: Boolean) = Indices(x)
  def _mkIndex(x: Array[Int]) = Indices(x)
  def _mkIndex(x: Array[Double]) = Indices(x)
  def _mkIndex(x: Array[Boolean]) = Indices(x)

  abstract class ApplyOrUpdate[A : ClassTag] {
    val x: Array[A]
    def apply(index: Indices): Array[A] = index.x.map(x(_))
    def apply(index: Index): A = x(index.x)
    def update(index: Indices, rhs: A): Unit = index.x.foreach(i => x(i) = rhs)
    def update[X: ClassTag](index: Indices, rhs: Array[A]): Unit = index.x.zipWithIndex.foreach(z => x(z._1) = rhs(z._2))
    def update(index: Index, rhs: A): Unit = x(index.x) = rhs
  }

  implicit class RScalaBooleanArray(val x: Array[Boolean]) extends ApplyOrUpdate[Boolean]

  implicit class RScalaStringArray(val x: Array[String]) extends ApplyOrUpdate[String]

  implicit class RScalaIntArray(val x: Array[Int]) extends ApplyOrUpdate[Int] {

    def unary_+ : Array[Int] = x.map(+_)
    def unary_- : Array[Int] = x.map(-_)
    def +(y: Double): Array[Double] = x.map(_+y)
    def -(y: Double): Array[Double] = x.map(_-y)
    def *(y: Double): Array[Double] = x.map(_*y)
    def /(y: Double): Array[Double] = x.map(_/y)
    def +(y: Int): Array[Int] = x.map(_+y)
    def -(y: Int): Array[Int] = x.map(_-y)
    def *(y: Int): Array[Int] = x.map(_*y)
    def /(y: Int): Array[Double] = {
      val yy = y.toDouble
      x.map(_/yy)
    }
    def +(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 + zz._2 }
    def -(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 - zz._2 }
    def *(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 * zz._2 }
    def /(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 / zz._2 }
    def +(y: Array[Int]): Array[Int] = x zip y map { zz => zz._1 + zz._2 }
    def -(y: Array[Int]): Array[Int] = x zip y map { zz => zz._1 - zz._2 }
    def *(y: Array[Int]): Array[Int] = x zip y map { zz => zz._1 * zz._2 }
    def /(y: Array[Int]): Array[Double] = x zip y map { zz => zz._1.toDouble / zz._2 }

  }

  implicit class RScalaDoubleArray(val x: Array[Double]) extends ApplyOrUpdate[Double] {

    def unary_+ : Array[Double] = x.map(+_)
    def unary_- : Array[Double] = x.map(-_)
    def +(y: Double): Array[Double] = x.map(_+y)
    def -(y: Double): Array[Double] = x.map(_-y)
    def *(y: Double): Array[Double] = x.map(_*y)
    def /(y: Double): Array[Double] = x.map(_/y)
    def +(y: Int): Array[Double] = x.map(_+y)
    def -(y: Int): Array[Double] = x.map(_-y)
    def *(y: Int): Array[Double] = x.map(_*y)
    def /(y: Int): Array[Double] = x.map(_/y)
    def +(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 + zz._2 }
    def -(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 - zz._2 }
    def *(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 * zz._2 }
    def /(y: Array[Double]): Array[Double] = x zip y map { zz => zz._1 / zz._2 }
    def +(y: Array[Int]): Array[Double] = x zip y map { zz => zz._1 + zz._2 }
    def -(y: Array[Int]): Array[Double] = x zip y map { zz => zz._1 - zz._2 }
    def *(y: Array[Int]): Array[Double] = x zip y map { zz => zz._1 * zz._2 }
    def /(y: Array[Int]): Array[Double] = x zip y map { zz => zz._1 / zz._2 }

  }

  implicit class RScalaInt(x: Int) {

    def +(y: Array[Int]): Array[Int] = y.map(x+_)
    def -(y: Array[Int]): Array[Int] = y.map(x-_)
    def *(y: Array[Int]): Array[Int] = y.map(x*_)
    def /(y: Array[Int]): Array[Double] = {
      val xx = x.toDouble
      y.map(xx/_)
    }

  }

  implicit class RScalaDouble(x: Double) {

    def +(y: Array[Double]): Array[Double] = y.map(x+_)
    def -(y: Array[Double]): Array[Double] = y.map(x-_)
    def *(y: Array[Double]): Array[Double] = y.map(x*_)
    def /(y: Array[Double]): Array[Double] = y.map(x/_)
    def +(y: Array[Int]): Array[Double] = y.map(x+_)
    def -(y: Array[Int]): Array[Double] = y.map(x-_)
    def *(y: Array[Int]): Array[Double] = y.map(x*_)
    def /(y: Array[Int]): Array[Double] = y.map(x/_)

  }

}

