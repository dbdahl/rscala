package org.ddahl.rscala

import java.net._
import java.io._
import scala.language.dynamics

import Protocol._

/** An interface to an R interpreter.
*
* An object `R` is the instance of this class available in a Scala interpreter created by calling the function
* `scala` from the package [[http://cran.r-project.org/package=rscala rscala]].  It is through this instance `R` that
* callbacks to the original [[http://www.r-project.org R]] interpreter are possible.
* 
* In a JVM-based application, an instance of this class is created using its companion object.  See below.  The paths of the
* rscala's JARs (for all supported versions of Scala) are available from [[http://www.r-project.org R]] using `rscala::rscalaJar()`.
* To get just the JAR for Scala 2.11, for example, use `rscala::rscalaJar("2.11")`.
* 
* {{{
* val R = org.ddahl.rscala.RClient()

* val a = R.evalD0("rnorm(8)")
* val b = R.evalD1("rnorm(8)")
* val c = R.evalD2("matrix(rnorm(8),nrow=4)")

* R eval """
*   v <- rbinom(8,size=10,prob=0.4)
*   m <- matrix(v,nrow=4)
* """
* val v1 = R.get("v")
* val v2 = R.get("v")._1.asInstanceOf[Array[Int]]   // This works, but is not very convenient
* val v3 = R.v._1.asInstanceOf[Array[Int]]          // Slightly better
* val v4 = R.getI0("v")   // Get the first element of R's "v" as a Int
* val v5 = R.getI1("v")   // Get R's "v" as an Array[Int]
* val v6 = R.getI2("m")   // Get R's "m" as an Array[Array[Int]]
* }}}
*
* @author David B. Dahl
*/
class RClient private (private val scalaServer: ScalaServer, private val in: DataInputStream, private val out: DataOutputStream, private val debugger: Debugger, private val serializeOutput: Boolean) extends Dynamic {

  /** __For rscala developers only__: Returns `TRUE` if debugging output is enabled. */
  def debug = debugger.value

  /** Closes the interface to the R interpreter.
  * 
  * Subsequent calls to the other methods will fail.
  */
  def exit() = {
    out.writeInt(SHUTDOWN)
    out.flush()
  }

  /** Evaluates `snippet` in the R interpreter.
  *
  * Returns `null` if `evalOnly`.  If `!evalOnly`, the last result of the R expression is converted if possible.
  * Conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.  The static type of the result, however, is `Any` so using the
  * method `evalXY` (where `X` is `I`, `D`, `B`, or `S` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.
  * [[evalD0]]).
  */
  def eval(snippet: String, evalOnly: Boolean = true): Any = {
    if ( debug ) debugger.msg("Sending EVAL request.")
    out.writeInt(if(serializeOutput) EVAL else EVALNAKED)
    Helper.writeString(out,if ( RClient.isWindows ) snippet.replaceAll("\r\n","\n") else snippet)
    out.flush()
    if ( scalaServer != null ) {
      if ( debug ) debugger.msg("Spinning up Scala server.")
      scalaServer.run()
      if ( debug ) debugger.msg("Spinning down Scala server.")
    }
    val status = in.readInt()
    if ( debug ) debugger.msg("Status is: "+status)
    val output = Helper.readString(in)
    if ( output != "" ) {
      println(output)
    } else if ( debug ) debugger.msg("No output.")
    if ( status != OK ) throw new RuntimeException("Error in R evaluation.")
    if ( evalOnly ) null else get(".rscala.last.value")._1
  }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI0]].  */
  def evalI0(snippet: String) = { eval(snippet,true); getI0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD0]].  */
  def evalD0(snippet: String) = { eval(snippet,true); getD0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getB0]].  */
  def evalB0(snippet: String) = { eval(snippet,true); getB0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS0]].  */
  def evalS0(snippet: String) = { eval(snippet,true); getS0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI1]].  */
  def evalI1(snippet: String) = { eval(snippet,true); getI1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD1]].  */
  def evalD1(snippet: String) = { eval(snippet,true); getD1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getB1]].  */
  def evalB1(snippet: String) = { eval(snippet,true); getB1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS1]].  */
  def evalS1(snippet: String) = { eval(snippet,true); getS1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI2]].  */
  def evalI2(snippet: String) = { eval(snippet,true); getI2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD2]].  */
  def evalD2(snippet: String) = { eval(snippet,true); getD2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getB2]].  */
  def evalB2(snippet: String) = { eval(snippet,true); getB2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS2]].  */
  def evalS2(snippet: String) = { eval(snippet,true); getS2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR]].  */
  def evalR( snippet: String) = { eval(snippet,true); getR( ".rscala.last.value") }

  /** A short-hand way to call [[get]].
  *
  * {{{
  * R eval """
  *   a <- numeric(10)
  *   for ( i in 2:length(a) ) {
  *     a[i] <- 0.5*a[i-1] + rnorm(1)
  *   }
  * """
  * R.a
  * }}}
  */
  def selectDynamic(identifier: String): (Any,String) = get(identifier)

  /** A short-hand way to call '''`set(identifier,value)`'''
  *
  * {{{
  * R.b = Array.fill(10) { scala.math.random }
  * }}}
  */
  def updateDynamic(identifier: String)(value : Any): Unit = set(identifier,value)
  
  /** Equivalent to calling '''`set(identifier, value, "", true)`'''. */
  def set(identifier: String, value: Any): Unit = set(identifier,value,"",true)

  /** Assigns `value` to a variable `identifier` in the R interpreter.
  * 
  * Integers, doubles, Booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.
  * 
  * If `index != ""`, assigned into elements of `identifier` are performed by using either single brackets
  * (`singleBrackets=true`) or double brackets (`singleBrackets=false`).
  *
  * {{{
  * R.a = Array(5,6,4)
  *
  * R.eval("b <- matrix(NA,nrow=3,ncol=2)")
  * for ( i <- 0 until 3 ) {
  *   R.set("b",Array(2*i,2*i+1),s"\${i+1},")
  * }
  * R.b
  *
  * R.eval("myList <- list()")
  * R.set("myList",Array("David","Grace","Susan"),"'names'",false)
  * R.set("myList",Array(5,4,5),"'counts'",false)
  * R.eval("print(myList)")
  * }}}
  */
  def set(identifier: String, value: Any, index: String = "", singleBrackets: Boolean = true): Unit = {
    if ( debug ) debugger.msg("Setting: "+identifier)
    val v = value
    if ( index == "" ) out.writeInt(SET)
    else if ( singleBrackets ) {
      out.writeInt(SET_SINGLE)
      Helper.writeString(out,index)
    } else {
      out.writeInt(SET_DOUBLE)
      Helper.writeString(out,index)
    }
    Helper.writeString(out,identifier)
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debug ) debugger.msg("... which is null")
      out.writeInt(NULLTYPE)
      out.flush()
      if ( index != "" ) {
        val status = in.readInt()
        if ( status != OK ) {
          val output = Helper.readString(in)
          if ( output != "" ) println(output)
          throw new RuntimeException("Error in R evaluation.")
        }
      }
      return
    }
    val c = v.getClass
    if ( debug ) debugger.msg("... whose class is: "+c)
    if ( debug ) debugger.msg("... and whose value is: "+v)
    if ( c.isArray ) {
      c.getName match {
        case "[I" =>
          val vv = v.asInstanceOf[Array[Int]]
          out.writeInt(VECTOR)
          out.writeInt(vv.length)
          out.writeInt(INTEGER)
          for ( i <- 0 until vv.length ) out.writeInt(vv(i))
        case "[D" =>
          val vv = v.asInstanceOf[Array[Double]]
          out.writeInt(VECTOR)
          out.writeInt(vv.length)
          out.writeInt(DOUBLE)
          for ( i <- 0 until vv.length ) out.writeDouble(vv(i))
        case "[Z" =>
          val vv = v.asInstanceOf[Array[Boolean]]
          out.writeInt(VECTOR)
          out.writeInt(vv.length)
          out.writeInt(BOOLEAN)
          for ( i <- 0 until vv.length ) out.writeInt(if ( vv(i) ) 1 else 0)
        case "[Ljava.lang.String;" =>
          val vv = v.asInstanceOf[Array[String]]
          out.writeInt(VECTOR)
          out.writeInt(vv.length)
          out.writeInt(STRING)
          for ( i <- 0 until vv.length ) Helper.writeString(out,vv(i))
        case "[[I" =>
          val vv = v.asInstanceOf[Array[Array[Int]]]
          if ( Helper.isMatrix(vv) ) {
            out.writeInt(MATRIX)
            out.writeInt(vv.length)
            if ( vv.length > 0 ) out.writeInt(vv(0).length)
            else out.writeInt(0)
            out.writeInt(INTEGER)
            for ( i <- 0 until vv.length ) {
              val vvv = vv(i)
              for ( j <- 0 until vvv.length ) {
                out.writeInt(vv(i)(j))
              }
            }
          }
        case "[[D" =>
          val vv = v.asInstanceOf[Array[Array[Double]]]
          if ( Helper.isMatrix(vv) ) {
            out.writeInt(MATRIX)
            out.writeInt(vv.length)
            if ( vv.length > 0 ) out.writeInt(vv(0).length)
            else out.writeInt(0)
            out.writeInt(DOUBLE)
            for ( i <- 0 until vv.length ) {
              val vvv = vv(i)
              for ( j <- 0 until vvv.length ) {
                out.writeDouble(vvv(j))
              }
            }
          } else out.writeInt(UNSUPPORTED_STRUCTURE)
        case "[[Z" =>
          val vv = v.asInstanceOf[Array[Array[Boolean]]]
          if ( Helper.isMatrix(vv) ) {
            out.writeInt(MATRIX)
            out.writeInt(vv.length)
            if ( vv.length > 0 ) out.writeInt(vv(0).length)
            else out.writeInt(0)
            out.writeInt(BOOLEAN)
            for ( i <- 0 until vv.length ) {
              val vvv = vv(i)
              for ( j <- 0 until vv(i).length ) {
                out.writeInt(if ( vvv(j) ) 1 else 0)
              }
            }
          } else out.writeInt(UNSUPPORTED_STRUCTURE)
        case "[[Ljava.lang.String;" =>
          val vv = v.asInstanceOf[Array[Array[String]]]
          if ( Helper.isMatrix(vv) ) {
            out.writeInt(MATRIX)
            out.writeInt(vv.length)
            if ( vv.length > 0 ) out.writeInt(vv(0).length)
            else out.writeInt(0)
            out.writeInt(STRING)
            for ( i <- 0 until vv.length ) {
              val vvv = vv(i)
              for ( j <- 0 until vv(i).length ) {
                Helper.writeString(out,vvv(j))
              }
            }
          } else out.writeInt(UNSUPPORTED_STRUCTURE)
        case _ =>
          throw new RuntimeException("Unsupported array type: "+c.getName)
      }
    } else {
      c.getName match {
        case "java.lang.Integer" =>
          out.writeInt(ATOMIC)
          out.writeInt(INTEGER)
          out.writeInt(v.asInstanceOf[Int])
        case "java.lang.Double" =>
          out.writeInt(ATOMIC)
          out.writeInt(DOUBLE)
          out.writeDouble(v.asInstanceOf[Double])
        case "java.lang.Boolean" =>
          out.writeInt(ATOMIC)
          out.writeInt(BOOLEAN)
          out.writeInt(if (v.asInstanceOf[Boolean]) 1 else 0)
        case "java.lang.String" =>
          out.writeInt(ATOMIC)
          out.writeInt(STRING)
          Helper.writeString(out,v.asInstanceOf[String])
        case _ =>
          throw new RuntimeException("Unsupported non-array type: "+c.getName)
      }
    }
    out.flush()
    if ( index != "" ) {
      val status = in.readInt()
      if ( status != OK ) {
        val output = Helper.readString(in)
        if ( output != "" ) println(output)
        throw new RuntimeException("Error in R evaluation.")
      }
    }
  }

  /** Returns the value of `identifier` in the R interpreter.  The static type of the result is `(Any,String)`, where
  * the first element is the value and the second is the runtime type.
  *
  * If `asReference=false`, conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e.
  * arrays) and matrices (i.e. retangular arrays of arrays) of these types.    Using the method `getXY` (where `X` is
  * `I`, `D`, `B`, or `S` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.  [[getD0]]).
  *
  * If `asReference=true`, the value is merely wrapped using [[RObject]] and objects of any type are supported.  Using
  * the method [[getR]] may be more convenient.
  */
  def get(identifier: String, asReference: Boolean = false): (Any,String) = {
    if ( debug ) debugger.msg("Getting: "+identifier)
    out.writeInt(if ( asReference ) GET_REFERENCE else GET)
    Helper.writeString(out,identifier)
    out.flush()
    if ( asReference ) {
      val r = in.readInt() match {
        case REFERENCE => (RObject(Helper.readString(in)),"RObject")
        case UNDEFINED_IDENTIFIER =>
          throw new RuntimeException("Undefined identifier")
      }
      return r
    }
    in.readInt match {
      case NULLTYPE =>
        if ( debug ) debugger.msg("Getting null.")
        (null,"Null")
      case ATOMIC =>
        if ( debug ) debugger.msg("Getting atomic.")
        in.readInt() match {
          case INTEGER => (in.readInt(),"Int")
          case DOUBLE => (in.readDouble(),"Double")
          case BOOLEAN => (( in.readInt() != 0 ),"Boolean")
          case STRING => (Helper.readString(in),"String")
          case _ => throw new RuntimeException("Protocol error")
        }
      case VECTOR =>
        if ( debug ) debugger.msg("Getting vector...")
        val length = in.readInt()
        if ( debug ) debugger.msg("... of length: "+length)
        in.readInt() match {
          case INTEGER => (Array.fill(length) { in.readInt() },"Array[Int]")
          case DOUBLE => (Array.fill(length) { in.readDouble() },"Array[Double]")
          case BOOLEAN => (Array.fill(length) { ( in.readInt() != 0 ) },"Array[Boolean]")
          case STRING => (Array.fill(length) { Helper.readString(in) },"Array[String]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case MATRIX =>
        if ( debug ) debugger.msg("Getting matrix...")
        val nrow = in.readInt()
        val ncol = in.readInt()
        if ( debug ) debugger.msg("... of dimensions: "+nrow+","+ncol)
        in.readInt() match {
          case INTEGER => (Array.fill(nrow) { Array.fill(ncol) { in.readInt() } },"Array[Array[Int]]")
          case DOUBLE => (Array.fill(nrow) { Array.fill(ncol) { in.readDouble() } },"Array[Array[Double]]")
          case BOOLEAN => (Array.fill(nrow) { Array.fill(ncol) { ( in.readInt() != 0 ) } },"Array[Array[Boolean]]")
          case STRING => (Array.fill(nrow) { Array.fill(ncol) { Helper.readString(in) } },"Array[Array[String]]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case UNDEFINED_IDENTIFIER => throw new RuntimeException("Undefined identifier")
      case UNSUPPORTED_STRUCTURE => throw new RuntimeException("Unsupported data type")
      case _ => throw new RuntimeException("Protocol error")
    }
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Int`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getI0(identifier: String): Int = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int]
    case (a,"Double") => a.asInstanceOf[Double].toInt
    case (a,"Boolean") => if (a.asInstanceOf[Boolean]) 1 else 0
    case (a,"String") => a.asInstanceOf[String].toInt
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0).toInt
    case (a,"Array[Boolean]") => if ( a.asInstanceOf[Array[Boolean]](0) ) 1 else 0
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toInt
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Int")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to a `Double`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getD0(identifier: String): Double = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int].toDouble
    case (a,"Double") => a.asInstanceOf[Double]
    case (a,"Boolean") => if (a.asInstanceOf[Boolean]) 1.0 else 0.0
    case (a,"String") => a.asInstanceOf[String].toDouble
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0).toDouble
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0)
    case (a,"Array[Boolean]") => if ( a.asInstanceOf[Array[Boolean]](0) ) 1.0 else 0.0
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toDouble
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Double")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to a `Boolean`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getB0(identifier: String): Boolean = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int] != 0
    case (a,"Double") => a.asInstanceOf[Double] != 0.0
    case (a,"Boolean") => a.asInstanceOf[Boolean]
    case (a,"String") => a.asInstanceOf[String].toLowerCase != "false"
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0) != 0
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0) != 0.0
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]](0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toLowerCase != "false"
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Boolean")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to a `string`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getS0(identifier: String): String = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int].toString
    case (a,"Double") => a.asInstanceOf[Double].toString
    case (a,"Boolean") => a.asInstanceOf[Boolean].toString
    case (a,"String") => a.asInstanceOf[String]
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0).toString
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0).toString
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]](0).toString
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to String")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Int]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getI1(identifier: String): Array[Int] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int])
    case (a,"Double") => Array(a.asInstanceOf[Double].toInt)
    case (a,"Boolean") => Array(if (a.asInstanceOf[Boolean]) 1 else 0)
    case (a,"String") => Array(a.asInstanceOf[String].toInt)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]]
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_.toInt)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(x => if (x) 1 else 0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toInt)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Int]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Double]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getD1(identifier: String): Array[Double] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int].toDouble)
    case (a,"Double") => Array(a.asInstanceOf[Double])
    case (a,"Boolean") => Array(if (a.asInstanceOf[Boolean]) 1.0 else 0.0)
    case (a,"String") => Array(a.asInstanceOf[String].toDouble)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_.toDouble)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]]
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(x => if (x) 1.0 else 0.0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toDouble)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Double]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Boolean]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getB1(identifier: String): Array[Boolean] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int] != 0)
    case (a,"Double") => Array(a.asInstanceOf[Double] != 0.0)
    case (a,"Boolean") => Array(a.asInstanceOf[Boolean])
    case (a,"String") => Array(a.asInstanceOf[String].toLowerCase != "false")
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_ != 0)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_ != 0.0)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]]
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toLowerCase != "false")
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Boolean]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[string]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getS1(identifier: String): Array[String] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int].toString)
    case (a,"Double") => Array(a.asInstanceOf[Double].toString)
    case (a,"Boolean") => Array(a.asInstanceOf[Boolean].toString)
    case (a,"String") => Array(a.asInstanceOf[String])
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_.toString)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_.toString)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(_.toString)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]]
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[String]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[Int]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getI2(identifier: String): Array[Array[Int]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]]
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]].map(_.map(x => if (x) 1 else 0))
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toInt))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[Int]]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[Double]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getD2(identifier: String): Array[Array[Double]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]].map(_.map(_.toDouble))
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]]
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]].map(_.map(x => if (x) 1.0 else 0.0))
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toDouble))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[Double]]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[Boolean]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getB2(identifier: String): Array[Array[Boolean]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]].map(_.map(_ != 0))
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]].map(_.map(_ != 0.0))
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]]
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toLowerCase != "false"))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[Boolean]]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[string]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getS2(identifier: String): Array[Array[String]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]].map(_.map(_.toString))
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]].map(_.map(_.toString))
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]].map(_.map(_.toString))
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]]
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[String]]")
  }

  /** Calls '''`get(identifier,true)`''' and converts the result to an `[[RObject]]`.
  *
  * The value is merely wrapped using [[RObject]] and objects of any type are supported.
  */
  def getR(identifier: String): RObject = get(identifier,true) match {
    case (a,"RObject") => a.asInstanceOf[RObject]
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to RObject")
  }

  /**
  * Reclaims memory associated with __'''all'''__ R references, including any instances of [[RObject]] that are still in
  * memory.
  */
  def gc(): Unit = {
    if ( debug ) debugger.msg("Sending GC request.")
    out.writeInt(GC)
    out.flush()
  }

}

/** The companion object to the [[RClient]] class used to create an instance of the [[RClient]] class in a JVM-based
 * application.
*
* An object `R` is an [[RClient]] instance available in a Scala interpreter created by calling the function
* `scala` from the package [[http://cran.r-project.org/package=rscala rscala]].  It is through this instance
* `R` that callbacks to the original [[http://www.r-project.org R]] interpreter are possible.
* 
* The paths of the rscala's JARs, for both Scala 2.10 and 2.11, are available from [[http://www.r-project.org R]] using
* `rscala::rscalaJar()`.  To get just the JAR for Scala 2.11, for example, use `rscala::rscalaJar("2.11")`.
* 
* {{{ val R = org.ddahl.rscala.RClient() }}}
*/
object RClient {

  import scala.sys.process._

  private val isWindows: Boolean = sys.props("os.name").toLowerCase match {
    case s if s.startsWith("""windows""") => true
    case s if s.startsWith("""linux""") => false
    case s if s.startsWith("""unix""") => false
    case s if s.startsWith("""mac""") => false
    case _ => throw new RuntimeException("Unrecognized OS")
  }

  private val defaultArguments = isWindows match {
    case true  => Array[String]("--no-save","--no-restore","--silent","--slave") 
    case false => Array[String]("--no-save","--no-restore","--silent","--slave")
  }

  private val interactiveArguments = isWindows match {
    case true  => Array[String]("--ess") 
    case false => Array[String]("--interactive")
  }

  private lazy val defaultRCmd = isWindows match {
    case true  => findROnWindows
    case false => """R"""
  }

  def findROnWindows: String = {
    def checkValidPath(path: String) = {
      val file = new java.io.File(path + """\bin\R.exe""")
      if ( file.exists && file.isFile ) file.getAbsolutePath
      else ""
    }
    def versionCompare(a: String, b: String): Boolean = { 
      val Seq(aa,bb) = Seq(a,b).map(_.takeWhile(_!='.'))
      val Seq(al,bl) = Seq(a,b).map(_.length)
      val Seq(aaa,bbb) = Seq(aa,bb).map(_.toInt)
      val Seq(aal,bbl) = Seq(aa,bb).map(_.length)
      if ( aaa == bbb ) { 
        if ( ( al == aal ) && ( bl == bbl ) ) false
        else if ( ( al == aal ) && ( bl != bbl ) ) true
        else if ( ( al != aal ) && ( bl == bbl ) ) false
        else versionCompare(a.substring(aal+1),b.substring(bbl+1))
      }
      else aaa < bbb 
    }
    def getPath(key: String): Option[String] = {
      val cmd = "reg query " + key
      try {
        val result1 = cmd.!!.trim.split("\\r\\n").map(_.trim)
        val result2 = result1.filter(_.matches("^\\s*InstallPath.*"))
        if ( result2.length == 1 ) {
          val result3 = checkValidPath(result2(0).split("REG_SZ")(1).trim)
          if ( result3 != "" ) Some(result3)
          else None
        } else {
          val dir = result1.length match {
            case 0 => ""
            case 1 => result1.head
            case _ => result1.zip(result1.map(x => new File(x).getName)).sortWith( (x,y) => {
                versionCompare(x._2,y._2)
              }).last._1
          }
          getPath(dir)
        }
      } catch {
        case _: Throwable => None
      }
    }
    for ( root <- List("HKEY_LOCAL_MACHINE","HKEY_CURRENT_USER") ) {
      val result = getPath(root+"""\SOFTWARE\R-core\R""")
      if ( result.isDefined ) return result.get
    }
    throw new RuntimeException("Cannot locate R using Windows registry.  Please explicitly specify its path.")
  }

  private def reader(debugger: Debugger, label: String)(input: InputStream) = {
    val in = new BufferedReader(new InputStreamReader(input))
    var line = in.readLine()
    while ( line != null ) {
      if ( debugger.value ) println(label+line)
      line = in.readLine()
    }
    in.close()
  }

  /**
  * Returns an instance of the [[RClient]] class by running the `R` executable in the path.
  */
  def apply(): RClient = apply(defaultRCmd)

  /**
  * Returns an instance of the [[RClient]] class by running the `R` executable in the path, specifying whether output
  * should be serialized.
  */
  def apply(serializeOutput: Boolean): RClient = apply(defaultRCmd,serializeOutput)

  /** Returns an instance of the [[RClient]] class, using the path specified by `rCmd` and specifying whether output
  * should be serialized, whether debugging output should be displayed, and the `timeout` to establish a connection
  * with the R interpreter.
  */
  def apply(rCmd: String, serializeOutput: Boolean = false, debug: Boolean = false, timeout: Int = 60): RClient = {
    var cmd: PrintWriter = null
    val command = rCmd +: ( defaultArguments ++ interactiveArguments )
    val processCmd = Process(command)
    val debugger = new Debugger(debug,new PrintWriter(System.out),"Scala",false)
    val processIO = new ProcessIO(
      o => { cmd = new PrintWriter(o) },
      reader(debugger,""),
      reader(debugger,""),
      true
    )
    val processInstance = processCmd.run(processIO)
    val codeInR = Seq("common.R","globals.R","protocol.R","rServer.R","scala.R","zzz.R").map(resource => {
      scala.io.Source.fromInputStream(getClass.getResourceAsStream("/R/"+resource)).getLines.mkString("\n")
    }).mkString("\n\n")
    val allCodeInR = s"""
      rscala <- local({
        ${codeInR}
        environment()
      })"""
    val sourceFile = File.createTempFile("rscala-","")
    val sourceFileNameForR = sourceFile.getAbsolutePath.replace(File.separator,"/")
    val writer = new FileWriter(sourceFile)
    writer.write(allCodeInR)
    writer.flush()
    writer.close()
    val portsFile = File.createTempFile("rscala-","")
    val snippet = s"""
      source("${sourceFileNameForR}")
      file.remove("${sourceFileNameForR}")
      rscala[['rServe']](rscala[['newSockets']]('${portsFile.getAbsolutePath.replace(File.separator,"/")}',debug=${if ( debug ) "TRUE" else "FALSE"},serialize=${if ( serializeOutput ) "TRUE" else "FALSE"},timeout=${timeout}),with.callbacks=FALSE)
      q(save='no')
    """.stripMargin
    while ( cmd == null ) Thread.sleep(100)
    cmd.println(snippet)
    cmd.flush()
    val sockets = new ScalaSockets(portsFile.getAbsolutePath,debugger)
    sockets.out.writeInt(OK)
    sockets.out.flush()
    apply(null,sockets.in,sockets.out,debugger,serializeOutput)
  }

  /** __For rscala developers only__: Returns an instance of the [[RClient]] class.  */
  def apply(scalaServer: ScalaServer, in: DataInputStream, out: DataOutputStream, debugger: Debugger, serializeOutput: Boolean): RClient = new RClient(scalaServer,in,out,debugger,serializeOutput)

}

