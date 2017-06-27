package org.ddahl.rscala

import java.net._
import java.io._
import scala.language.dynamics
import java.lang.ref.{Reference => JavaReference, PhantomReference, ReferenceQueue}

import server._
import Protocol._

/** An interface to an R interpreter.
*
* An object `R` is the instance of this class available in a Scala interpreter created by calling the function
* `scala` from the package [[http://cran.r-project.org/package=rscala rscala]].  It is through this instance `R` that
* callbacks to the original [[http://www.r-project.org R]] interpreter are possible.
* 
* In a Scala application, an instance of this class is created using its companion object.  See below.  The paths of the
* rscala's JARs (for all supported versions of Scala) are available from [[http://www.r-project.org R]] using `rscala::.rscalaJar()`.
* To get just the JAR for Scala 2.12, for example, use `rscala::.rscalaJar("2.12")`.
* 
* {{{
* val R = org.ddahl.rscala.RClient()

* val a = R.evalD0("rnorm(8)")
* val b = R.evalD1("rnorm(8)")
* val c = R.evalD2("matrix(rnorm(8),nrow=4)")

* R.set("ages", Array(4,2,7,9))
* R.ages = Array(4,2,7,9)
* println(R.getI1("ages").mkString("<",", ",">"))

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
class RClient private (private val scalaServer: ScalaServer, private val in: DataInputStream, private val out: DataOutputStream, private val debugger: Debugger, val serializeOutput: Boolean, val rowMajor: Boolean) extends Dynamic {

  import Helper.{readString, writeString, isMatrix, transposeIf}

  private val referenceQueue = new ReferenceQueue[PersistentReference]()
  private val referenceMap = new scala.collection.mutable.HashMap[JavaReference[_ <: PersistentReference],String]()

  /** __For rscala developers only__: Returns `TRUE` if debugging output is enabled. */
  def debug = debugger.value

  /** Closes the interface to the R interpreter.
  * 
  * Subsequent calls to the other methods will fail.
  */
  def exit() = {
    check4GC()
    out.writeInt(SHUTDOWN)
    out.flush()
  }

  private def check4GC() {
    val first = referenceQueue.poll
    if ( first != null ) {
      out.writeInt(FREE)
      var list = List[JavaReference[_ <: PersistentReference]](first)
      while ( list.head != null ) {
        list = referenceQueue.poll :: list
      }
      list = list.tail
      out.writeInt(list.size)
      list.foreach( x => {
        val id = referenceMap(x)
        referenceMap.remove(x)
        writeString(out,id)
      })
      out.flush()
    }
  }

  /** Evaluates `snippet` in the R interpreter.
  *
  * Returns `null` if `evalOnly`.  If `!evalOnly`, the last result of the R expression is converted if possible.
  * Conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.  The static type of the result, however, is `Any` so using the
  * method `evalXY` (where `X` is `I`, `D`, `B`, `S`, or `R` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.
  * [[evalD0]]).
  */
  def eval(snippet: String, evalOnly: Boolean): Any = {
    check4GC()
    if ( debug ) debugger.msg("Sending EVAL request.")
    out.writeInt(if(serializeOutput) EVAL else EVALNAKED)
    writeString(out,if ( RClient.isWindows ) snippet.replaceAll("\r\n","\n") else snippet)
    out.flush()
    if ( scalaServer != null ) {
      if ( debug ) debugger.msg("Spinning up Scala server.")
      scalaServer.run()
      if ( debug ) debugger.msg("Spinning down Scala server.")
    }
    val status = in.readInt()
    if ( debug ) debugger.msg("Status is: "+status)
    val output = readString(in)
    if ( output != "" ) {
      println(output)
    } else if ( debug ) debugger.msg("No output.")
    if ( status != OK ) throw new RuntimeException("Error in R evaluation.")
    if ( evalOnly ) null else get(".rscala.last.value")._1
  }

  /** Calls '''`eval(snippet,true)`'''.  */
  def eval(snippet: String): Unit = eval(snippet,true)

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getReference]].  */
  def evalReference(snippet: String) = { eval(snippet,true); getReference(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI0]].  */
  def evalI0(snippet: String) = { eval(snippet,true); getI0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD0]].  */
  def evalD0(snippet: String) = { eval(snippet,true); getD0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL0]].  */
  def evalL0(snippet: String) = { eval(snippet,true); getL0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS0]].  */
  def evalS0(snippet: String) = { eval(snippet,true); getS0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR0]].  */
  def evalR0(snippet: String) = { eval(snippet,true); getR0(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI1]].  */
  def evalI1(snippet: String) = { eval(snippet,true); getI1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD1]].  */
  def evalD1(snippet: String) = { eval(snippet,true); getD1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL1]].  */
  def evalL1(snippet: String) = { eval(snippet,true); getL1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS1]].  */
  def evalS1(snippet: String) = { eval(snippet,true); getS1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR1]].  */
  def evalR1(snippet: String) = { eval(snippet,true); getR1(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI2]].  */
  def evalI2(snippet: String) = { eval(snippet,true); getI2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD2]].  */
  def evalD2(snippet: String) = { eval(snippet,true); getD2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL2]].  */
  def evalL2(snippet: String) = { eval(snippet,true); getL2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS2]].  */
  def evalS2(snippet: String) = { eval(snippet,true); getS2(".rscala.last.value") }

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR2]].  */
  def evalR2(snippet: String) = { eval(snippet,true); getR2(".rscala.last.value") }

  /** Invokes an R function with arguments.  */
  def invoke(function: Reference, args: Any*) = eval(mkSnippet(function,args))

  /** Invokes an R function with arguments.  */
  def invoke(functionName: String, args: Any*) = eval(mkSnippet(functionName,args))

  /** Invokes an R function with arguments and returns the result using [[getReference]].  */
  def invokeReference(function: Reference, args: Any*) = evalReference(mkSnippet(function,args))

  /** Invokes an R function with arguments and returns the result using [[getReference]].  */
  def invokeReference(functionName: String, args: Any*) = evalReference(mkSnippet(functionName,args))

  /** Invokes an R function with arguments and returns the result using [[getI0]].  */
  def invokeI0(function: Reference, args: Any*) = evalI0(mkSnippet(function,args))

  /** Invokes an R function with arguments and returns the result using [[getI0]].  */
  def invokeI0(functionName: String, args: Any*) = evalI0(mkSnippet(functionName,args))

  /** Invokes an R function with arguments and returns the result using [[getD0]].  */
  def invokeD0(function: Reference, args: Any*) = evalD0(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getD0]].  */
  def invokeD0(functionName: String, args: Any*) = evalD0(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL0]].  */
  def invokeL0(function: Reference, args: Any*) = evalL0(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL0]].  */
  def invokeL0(functionName: String, args: Any*) = evalL0(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS0]].  */
  def invokeS0(function: Reference, args: Any*) = evalS0(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS0]].  */
  def invokeS0(functionName: String, args: Any*) = evalS0(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR0]].  */
  def invokeR0(function: Reference, args: Any*) = evalR0(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR0]].  */
  def invokeR0(functionName: String, args: Any*) = evalR0(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getI1]].  */
  def invokeI1(function: Reference, args: Any*) = evalI1(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getI1]].  */
  def invokeI1(functionName: String, args: Any*) = evalI1(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getD1]].  */
  def invokeD1(function: Reference, args: Any*) = evalD1(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getD1]].  */
  def invokeD1(functionName: String, args: Any*) = evalD1(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL1]].  */
  def invokeL1(function: Reference, args: Any*) = evalL1(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL1]].  */
  def invokeL1(functionName: String, args: Any*) = evalL1(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS1]].  */
  def invokeS1(function: Reference, args: Any*) = evalS1(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS1]].  */
  def invokeS1(functionName: String, args: Any*) = evalS1(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR1]].  */
  def invokeR1(function: Reference, args: Any*) = evalR1(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR1]].  */
  def invokeR1(functionName: String, args: Any*) = evalR1(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getI2]].  */
  def invokeI2(function: Reference, args: Any*) = evalI2(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getI2]].  */
  def invokeI2(functionName: String, args: Any*) = evalI2(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getD2]].  */
  def invokeD2(function: Reference, args: Any*) = evalD2(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getD2]].  */
  def invokeD2(functionName: String, args: Any*) = evalD2(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL2]].  */
  def invokeL2(function: Reference, args: Any*) = evalL2(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getL2]].  */
  def invokeL2(functionName: String, args: Any*) = evalL2(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS2]].  */
  def invokeS2(function: Reference, args: Any*) = evalS2(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getS2]].  */
  def invokeS2(functionName: String, args: Any*) = evalS2(mkSnippet(functionName,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR2]].  */
  def invokeR2(function: Reference, args: Any*) = evalR2(mkSnippet(function,args))
  
  /** Invokes an R function with arguments and returns the result using [[getR2]].  */
  def invokeR2(functionName: String, args: Any*) = evalR2(mkSnippet(functionName,args))
  
  private def mkSnippet(functionName: String, args: Seq[Any]): String = mkSnippet(EphemeralReference(functionName), args)

  private def mkSnippet(function: Reference, args: Seq[Any]): String = {
    var counter = 0
    val argsStrings = args.map {
      case null => "NULL"
      case (name: String, r: Reference) => name + "=" + r.toString
      case (name: String, o) =>
        val id = ".rsX" + counter
        counter += 1
        set(id,o)
        name + "=" + id
      case r: Reference => r.toString
      case o =>
        val id = ".rsX" + counter
        counter += 1
        set(id,o)
        id
    }
    function + "(" + argsStrings.mkString(",") + ")"
  }

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
    check4GC()
    if ( debug ) debugger.msg("Setting: "+identifier)
    val v = value
    if ( index == "" ) out.writeInt(SET)
    else if ( singleBrackets ) {
      out.writeInt(SET_SINGLE)
      writeString(out,index)
    } else {
      out.writeInt(SET_DOUBLE)
      writeString(out,index)
    }
    writeString(out,identifier)
    var errorNote: Option[String] = None
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debug ) debugger.msg("... which is null")
      out.writeInt(NULLTYPE)
    } else {
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
            for ( i <- 0 until vv.length ) writeString(out,vv(i))
          case "[B" =>
            val vv = v.asInstanceOf[Array[Byte]]
            out.writeInt(VECTOR)
            out.writeInt(vv.length)
            out.writeInt(BYTE)
            for ( i <- 0 until vv.length ) out.writeByte(vv(i))
          case "[[I" =>
            val vv1 = v.asInstanceOf[Array[Array[Int]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIf(vv1, rowMajor)
              out.writeInt(MATRIX)
              out.writeInt(vv.length)
              if ( vv.length > 0 ) out.writeInt(vv(0).length)
              else out.writeInt(0)
              out.writeInt(INTEGER)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vvv.length ) {
                  out.writeInt(vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              out.writeInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[D" =>
            val vv1 = v.asInstanceOf[Array[Array[Double]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIf(vv1, rowMajor)
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
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              out.writeInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[Z" =>
            val vv1 = v.asInstanceOf[Array[Array[Boolean]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIf(vv1, rowMajor)
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
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              out.writeInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[Ljava.lang.String;" =>
            val vv1 = v.asInstanceOf[Array[Array[String]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIf(vv1, rowMajor)
              out.writeInt(MATRIX)
              out.writeInt(vv.length)
              if ( vv.length > 0 ) out.writeInt(vv(0).length)
              else out.writeInt(0)
              out.writeInt(STRING)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vv(i).length ) {
                  writeString(out,vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              out.writeInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[B" =>
            val vv1 = v.asInstanceOf[Array[Array[Byte]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIf(vv1, rowMajor)
              out.writeInt(MATRIX)
              out.writeInt(vv.length)
              if ( vv.length > 0 ) out.writeInt(vv(0).length)
              else out.writeInt(0)
              out.writeInt(BYTE)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vvv.length ) {
                  out.writeByte(vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              out.writeInt(UNSUPPORTED_STRUCTURE)
            }
          case _ =>
            errorNote = Some("Unsupported array type.")
            out.writeInt(UNSUPPORTED_STRUCTURE)
        }
      } else {
        c.getName match {
          case "java.lang.Integer" =>
            out.writeInt(SCALAR)
            out.writeInt(INTEGER)
            out.writeInt(v.asInstanceOf[Int])
          case "java.lang.Double" =>
            out.writeInt(SCALAR)
            out.writeInt(DOUBLE)
            out.writeDouble(v.asInstanceOf[Double])
          case "java.lang.Boolean" =>
            out.writeInt(SCALAR)
            out.writeInt(BOOLEAN)
            out.writeInt(if (v.asInstanceOf[Boolean]) 1 else 0)
          case "java.lang.String" =>
            out.writeInt(SCALAR)
            out.writeInt(STRING)
            writeString(out,v.asInstanceOf[String])
          case "java.lang.Byte" =>
            out.writeInt(SCALAR)
            out.writeInt(BYTE)
            out.writeByte(v.asInstanceOf[Byte])
          case _ =>
            errorNote = Some("Unsupported non-array type.")
            out.writeInt(UNSUPPORTED_STRUCTURE)
        }
      }
    }
    out.flush()
    if ( errorNote.isDefined ) {
      throw new RuntimeException(errorNote.get)
    }
    if ( index != "" ) {
      val status = in.readInt()
      if ( status != OK ) {
        val output = readString(in)
        if ( output != "" ) println(output)
        throw new RuntimeException("Error in R evaluation.")
      }
    }
  }

  /** __For rscala developers only__:  Returns a value previously cached for the R interpreter. */
  def cached(identifier: String): Any = {
    if ( identifier.startsWith(".") ) {
      scalaServer.cacheMap(identifier)._1
    } else {
      scalaServer.repl.valueOfTerm(identifier).get
    }
  }

  /** Returns the value of `identifier` in the R interpreter.  The static type of the result is `(Any,String)`, where
  * the first element is the value and the second is the runtime type.
  *
  * Conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e.
  * arrays) and matrices (i.e. retangular arrays of arrays) of these types.    Using the method `getXY` (where `X` is
  * `I`, `D`, `B`, or `S` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.  [[getD0]]).
  */
  def get(identifier: String, asReference: Boolean = false): (Any,String) = {
    check4GC()
    if ( debug ) debugger.msg("Getting: "+identifier)
    if ( asReference ) out.writeInt(GET_REFERENCE) else out.writeInt(GET)
    writeString(out,identifier)
    out.flush()
    in.readInt match {
      case NULLTYPE =>
        if ( debug ) debugger.msg("Getting null.")
        (null,"Null")
      case SCALAR =>
        if ( debug ) debugger.msg("Getting atomic.")
        in.readInt() match {
          case INTEGER => (in.readInt(),"Int")
          case DOUBLE => (in.readDouble(),"Double")
          case BOOLEAN => (( in.readInt() != 0 ),"Boolean")
          case STRING => (readString(in),"String")
          case BYTE => (in.readByte(),"Byte")
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
          case STRING => (Array.fill(length) { readString(in) },"Array[String]")
          case BYTE => (Array.fill(length) { in.readByte() },"Array[Byte]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case MATRIX =>
        if ( debug ) debugger.msg("Getting matrix...")
        val nrow = in.readInt()
        val ncol = in.readInt()
        if ( debug ) debugger.msg("... of dimensions: "+nrow+","+ncol)
        in.readInt() match {
          case INTEGER => ( transposeIf(Array.fill(nrow) { Array.fill(ncol) { in.readInt() } }, rowMajor),"Array[Array[Int]]")
          case DOUBLE => ( transposeIf(Array.fill(nrow) { Array.fill(ncol) { in.readDouble() } }, rowMajor),"Array[Array[Double]]")
          case BOOLEAN => ( transposeIf(Array.fill(nrow) { Array.fill(ncol) { ( in.readInt() != 0 ) } }, rowMajor),"Array[Array[Boolean]]")
          case STRING => ( transposeIf(Array.fill(nrow) { Array.fill(ncol) { readString(in) } }, rowMajor),"Array[Array[String]]")
          case BYTE => ( transposeIf(Array.fill(nrow) { Array.fill(ncol) { in.readByte() } }, rowMajor),"Array[Array[Byte]]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case REFERENCE =>
        if ( debug ) debugger.msg("Getting reference.")
        val reference = PersistentReference(readString(in))
        val phantomReference = new PhantomReference(reference, referenceQueue)
        referenceMap(phantomReference) = reference.name
        (reference, "org.ddahl.rscala.PersistentReference")
      case UNDEFINED_IDENTIFIER => throw new RuntimeException("Undefined identifier: "+identifier)
      case UNSUPPORTED_STRUCTURE => throw new RuntimeException("Unsupported data type: "+identifier)
      case e => throw new RuntimeException("Protocol error: Unknown type: "+e)
    }
  }

  /** Obtains a persistent R reference to the named object so that it can be accessed outside of the current environment. */
  def getReference(identifier: String): PersistentReference = get(identifier,true)._1.asInstanceOf[PersistentReference]

  /** Converts an ephemeral R reference to a persistent R reference so that it can be accessed outside of the current environment. */
  def getReference(reference: EphemeralReference): PersistentReference = getReference(reference.name)

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
    case (a,"Byte") => a.asInstanceOf[Byte].toInt
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0).toInt
    case (a,"Array[Boolean]") => if ( a.asInstanceOf[Array[Boolean]](0) ) 1 else 0
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toInt
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]](0).toInt
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
    case (a,"Byte") => a.asInstanceOf[Byte].toDouble
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0).toDouble
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0)
    case (a,"Array[Boolean]") => if ( a.asInstanceOf[Array[Boolean]](0) ) 1.0 else 0.0
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toDouble
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]](0).toDouble
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Double")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to a `Boolean`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getL0(identifier: String): Boolean = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int] != 0
    case (a,"Double") => a.asInstanceOf[Double] != 0.0
    case (a,"Boolean") => a.asInstanceOf[Boolean]
    case (a,"String") => a.asInstanceOf[String].toLowerCase != "false"
    case (a,"Byte") => a.asInstanceOf[Byte] != 0.toByte
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0) != 0
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0) != 0.0
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]](0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toLowerCase != "false"
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]](0) != 0.toByte
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
    case (a,"Byte") => a.asInstanceOf[Byte].toString
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0).toString
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0).toString
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]](0).toString
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0)
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]](0).toString
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to String")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to a `Byte`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getR0(identifier: String): Byte = get(identifier) match {
    case (a,"Int") => a.asInstanceOf[Int].toByte
    case (a,"Double") => a.asInstanceOf[Double].toByte
    case (a,"Boolean") => if (a.asInstanceOf[Boolean]) 1.toByte else 0.toByte
    case (a,"String") => a.asInstanceOf[String].toByte
    case (a,"Byte") => a.asInstanceOf[Byte]
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]](0).toByte
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]](0).toByte
    case (a,"Array[Boolean]") => if ( a.asInstanceOf[Array[Boolean]](0) ) 1.toByte else 0.toByte
    case (a,"Array[String]") => a.asInstanceOf[Array[String]](0).toByte
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]](0)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Byte")
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
    case (a,"Byte") => Array(a.asInstanceOf[Byte].toInt)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]]
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_.toInt)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(x => if (x) 1 else 0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toInt)
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]].map(_.toInt)
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
    case (a,"Byte") => Array(a.asInstanceOf[Byte].toDouble)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_.toDouble)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]]
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(x => if (x) 1.0 else 0.0)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toDouble)
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]].map(_.toDouble)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Double]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Boolean]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getL1(identifier: String): Array[Boolean] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int] != 0)
    case (a,"Double") => Array(a.asInstanceOf[Double] != 0.0)
    case (a,"Boolean") => Array(a.asInstanceOf[Boolean])
    case (a,"String") => Array(a.asInstanceOf[String].toLowerCase != "false")
    case (a,"Byte") => Array(a.asInstanceOf[Byte] != 0.toByte)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_ != 0)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_ != 0.0)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]]
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toLowerCase != "false")
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]].map(_ != 0.toByte)
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
    case (a,"Byte") => Array(a.asInstanceOf[Byte].toString)
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_.toString)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_.toString)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(_.toString)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]]
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]].map(_.toString)
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[String]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Byte]`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getR1(identifier: String): Array[Byte] = get(identifier) match {
    case (a,"Int") => Array(a.asInstanceOf[Int].toByte)
    case (a,"Double") => Array(a.asInstanceOf[Double].toByte)
    case (a,"Boolean") => Array(if (a.asInstanceOf[Boolean]) 1.toByte else 0.toByte)
    case (a,"String") => Array(a.asInstanceOf[String].toByte)
    case (a,"Byte") => Array(a.asInstanceOf[Byte])
    case (a,"Array[Int]") => a.asInstanceOf[Array[Int]].map(_.toByte)
    case (a,"Array[Double]") => a.asInstanceOf[Array[Double]].map(_.toByte)
    case (a,"Array[Boolean]") => a.asInstanceOf[Array[Boolean]].map(x => if (x) 1.toByte else 0.toByte)
    case (a,"Array[String]") => a.asInstanceOf[Array[String]].map(_.toByte)
    case (a,"Array[Byte]") => a.asInstanceOf[Array[Byte]]
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Byte]")
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
    case (a,"Array[Array[Byte]]") => a.asInstanceOf[Array[Array[Byte]]].map(_.map(_.toInt))
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
    case (a,"Array[Array[Byte]]") => a.asInstanceOf[Array[Array[Byte]]].map(_.map(_.toDouble))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[Double]]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[Boolean]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getL2(identifier: String): Array[Array[Boolean]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]].map(_.map(_ != 0))
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]].map(_.map(_ != 0.0))
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]]
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toLowerCase != "false"))
    case (a,"Array[Array[Byte]]") => a.asInstanceOf[Array[Array[Byte]]].map(_.map(_ != 0.toByte))
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
    case (a,"Array[Array[Byte]]") => a.asInstanceOf[Array[Array[Byte]]].map(_.map(_.toString))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[String]]")
  }

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Array[Array[Byte]]`.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, Booleans, and strings are supported.  Integers, doubles,
  * Booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  */
  def getR2(identifier: String): Array[Array[Byte]] = get(identifier) match {
    case (a,"Array[Array[Int]]") => a.asInstanceOf[Array[Array[Int]]].map(_.map(_.toByte))
    case (a,"Array[Array[Double]]") => a.asInstanceOf[Array[Array[Double]]].map(_.map(_.toByte))
    case (a,"Array[Array[Boolean]]") => a.asInstanceOf[Array[Array[Boolean]]].map(_.map(x => if (x) 1.toByte else 0.toByte))
    case (a,"Array[Array[String]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toByte))
    case (a,"Array[Array[Byte]]") => a.asInstanceOf[Array[Array[String]]].map(_.map(_.toByte))
    case (_,tp) => throw new RuntimeException(s"Unable to cast ${tp} to Array[Array[Byte]]")
  }

}

/** The companion object to the [[RClient]] class used to create an instance of the [[RClient]] class in a JVM-based
 * application.
*
* An object `R` is an [[RClient]] instance available in a Scala interpreter created by calling the function
* `scala` from the package [[http://cran.r-project.org/package=rscala rscala]].  It is through this instance
* `R` that callbacks to the original [[http://www.r-project.org R]] interpreter are possible.
* 
* The paths of the rscala's JARs are available from [[http://www.r-project.org R]] using
* `rscala::.rscalaJar()`.  To get just the JAR for Scala 2.12, for example, use `rscala::.rscalaJar("2.12")`.
* 
* {{{ val R = org.ddahl.rscala.RClient() }}}
*/
object RClient {

  import scala.sys.process._

  private val isWindows = scala.util.Properties.isWin

  private val defaultArguments = isWindows match {
    case true  => Array[String]("--no-save","--no-restore","--silent","--slave") 
    case false => Array[String]("--no-save","--no-restore","--silent","--slave")
  }

  private val interactiveArguments = isWindows match {
    case true  => Array[String]("--ess") 
    case false => Array[String]("--interactive")
  }

  def defaultRCmd = isWindows match {
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

  /** Returns an instance of the [[RClient]] class, using the path specified by `rCmd` and specifying whether output
  * should be serialized, whether matrices are row major, whether debugging output should be displayed, the `timeout` to establish a connection
  * with the R interpreter, and the port number.  Two sockets are establised using: 1. the specified port and 2. the specified port plus one.
  */
  def apply(rCmd: String = defaultRCmd, serializeOutput: Boolean = false, rowMajor: Boolean = true, port: Int = 0, debug: Boolean = false, timeout: Int = 60): RClient = {
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
      .rsI <- rscala[['newSockets']]('${portsFile.getAbsolutePath.replace(File.separator,"/")}',debug=${if ( debug ) "TRUE" else "FALSE"},serialize.output=${if ( serializeOutput ) "TRUE" else "FALSE"},row.major=${if ( rowMajor ) "TRUE" else "FALSE"},timeout=${timeout})
      rscala[['rServe']](.rsI,with.callbacks=FALSE)
      q(save='no')
    """.stripMargin
    while ( cmd == null ) Thread.sleep(100)
    cmd.println(snippet)
    cmd.flush()
    val sockets = new ScalaSockets(portsFile.getAbsolutePath,port,debugger)
    sockets.out.writeInt(OK)
    sockets.out.flush()
    apply(null,sockets.in,sockets.out,debugger,serializeOutput,rowMajor)
  }

  /** __For rscala developers only__: Returns an instance of the [[RClient]] class.  */
  def apply(scalaServer: ScalaServer, in: DataInputStream, out: DataOutputStream, debugger: Debugger, serializeOutput: Boolean, rowMajor: Boolean): RClient = new RClient(scalaServer,in,out,debugger,serializeOutput,rowMajor)

}

