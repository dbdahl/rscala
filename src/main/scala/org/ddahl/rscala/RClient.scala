package org.ddahl.rscala

import java.io.{File, FileWriter, PrintWriter, BufferedReader, InputStreamReader, InputStream}
import java.nio.channels.SocketChannel
import java.nio.ByteBuffer
import scala.language.dynamics
import java.lang.ref.{Reference => JavaReference, PhantomReference, ReferenceQueue}
import scala.sys.process.Process

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
* This class is threadsafe.
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
class RClient private (private val scalaServer: ScalaServer, private val rProcessInstance: Process, private val sc: SocketChannel, private val buffer: ByteBuffer, private val debugger: Debugger, val serializeOutput: Boolean, val rowMajor: Boolean) extends Dynamic {

  import Helper.{readString, writeString, isMatrix, transposeIfNot}

  private val referenceQueue = new ReferenceQueue[PersistentReference]()
  private val referenceMap = new scala.collection.mutable.HashMap[JavaReference[_ <: PersistentReference],String]()

  /** __For rscala developers only__: Returns `TRUE` if debugging output is enabled. */
  def debug = debugger.value

  /** Pings the R interpreter.
  *
  * Calling this method periodically on an otherwise-idle client may prevent the operating system from closing the socket.
  * Returns `true` if the ping is successful and `false` otherwise.
  */
  def ping(): Boolean = synchronized {
    try {
      buffer.putInt(PING)
      buffer.flip()
      sc.write(buffer)
      val status = buffer.getInt()
      status == OK
    } catch {
      case _ : Throwable => false
    }
  }

  /** Closes the interface to the R interpreter.
  * 
  * Subsequent calls to the other methods will fail.
  */
  def exit(): Unit = synchronized {
    try {
      check4GC()
      buffer.putInt(SHUTDOWN)
      buffer.flip()
      sc.write(buffer)
    } catch {
      case e : Throwable =>
        if ( rProcessInstance != null ) rProcessInstance.destroy()
        else throw e
    }
  }

  private def check4GC() = {
    val first = referenceQueue.poll
    if ( first != null ) {
      buffer.putInt(FREE)
      var list = List[JavaReference[_ <: PersistentReference]](first)
      while ( list.head != null ) {
        list = referenceQueue.poll :: list
      }
      list = list.tail
      buffer.putInt(list.size)
      list.foreach( x => {
        val id = referenceMap(x)
        referenceMap.remove(x)
        writeString(buffer,id)
      })
      buffer.flip()
      sc.write(buffer)
    }
  }

  /** Evaluates `snippet` in the R interpreter.
  *
  * Returns `null` if `evalOnly`.  If `!evalOnly`, the last result of the R expression is returned.  The result is
  * converted if `asReference` is `false` and the conversion is supported, otherwise a reference is returned.
  * Conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.  The static type of the result, however, is `Any` so using the
  * method `evalXY` (where `X` is `I`, `D`, `B`, `S`, or `R` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.
  * [[evalD0]]).
  */
  def eval(snippet: String, evalOnly: Boolean, asReference: Boolean): (Any, String) = synchronized {
    check4GC()
    if ( debug ) debugger.msg("Sending EVAL request.")
    buffer.putInt(if(serializeOutput) EVAL else EVALNAKED)
    writeString(buffer,if ( RClient.isWindows ) snippet.replaceAll("\r\n","\n") else snippet)
    buffer.flip()
    sc.write(buffer)
    if ( scalaServer != null ) {
      if ( debug ) debugger.msg("Spinning up Scala server.")
      scalaServer.run()
      if ( debug ) debugger.msg("Spinning down Scala server.")
    }
    val status = buffer.getInt()
    if ( debug ) debugger.msg("Status is: "+status)
    val output = readString(sc,buffer)
    if ( output != "" ) {
      println(output)
    } else if ( debug ) debugger.msg("No output.")
    if ( status != OK ) throw new RuntimeException("Error in R evaluation.")
    if ( evalOnly ) null else {
      if ( debug ) debugger.msg("Getting EVAL result.")
      if ( asReference ) buffer.putInt(GET_REFERENCE) else buffer.putInt(GET)
      writeString(buffer,".rsX")
      buffer.flip()
      sc.write(buffer)
      getInternal()
    }
  }

  /** Calls '''`eval(snippet,true)`'''.  */
  def eval(snippet: String): Unit = eval(snippet,true,false)

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getReference]].  */
  def evalReference(snippet: String) = toReference(eval(snippet,false,true))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI0]].  */
  def evalI0(snippet: String) = toI0(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD0]].  */
  def evalD0(snippet: String) = toD0(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL0]].  */
  def evalL0(snippet: String) = toL0(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS0]].  */
  def evalS0(snippet: String) = toS0(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR0]].  */
  def evalR0(snippet: String) = toR0(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI1]].  */
  def evalI1(snippet: String) = toI1(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD1]].  */
  def evalD1(snippet: String) = toD1(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL1]].  */
  def evalL1(snippet: String) = toL1(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS1]].  */
  def evalS1(snippet: String) = toS1(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR1]].  */
  def evalR1(snippet: String) = toR1(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getI2]].  */
  def evalI2(snippet: String) = toI2(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getD2]].  */
  def evalD2(snippet: String) = toD2(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getL2]].  */
  def evalL2(snippet: String) = toL2(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getS2]].  */
  def evalS2(snippet: String) = toS2(eval(snippet,false,false))

  /** Calls '''`eval(snippet,true)`''' and returns the result using [[getR2]].  */
  def evalR2(snippet: String) = toR2(eval(snippet,false,false))

  /** Invokes an R function with arguments.  */
  def invoke(function: Reference, args: Any*): Unit = synchronized { eval(mkSnippet(function,args)) }

  /** Invokes an R function with arguments.  */
  def invoke(functionName: String, args: Any*): Unit = synchronized { eval(mkSnippet(functionName,args)) }

  /** Invokes an R function with arguments and returns the result using [[getReference]].  */
  def invokeReference(function: Reference, args: Any*) = synchronized { evalReference(mkSnippet(function,args)) }

  /** Invokes an R function with arguments and returns the result using [[getReference]].  */
  def invokeReference(functionName: String, args: Any*) = synchronized { evalReference(mkSnippet(functionName,args)) }

  /** Invokes an R function with arguments and returns the result using [[getI0]].  */
  def invokeI0(function: Reference, args: Any*) = synchronized { evalI0(mkSnippet(function,args)) }

  /** Invokes an R function with arguments and returns the result using [[getI0]].  */
  def invokeI0(functionName: String, args: Any*) = synchronized { evalI0(mkSnippet(functionName,args)) }

  /** Invokes an R function with arguments and returns the result using [[getD0]].  */
  def invokeD0(function: Reference, args: Any*) = synchronized { evalD0(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getD0]].  */
  def invokeD0(functionName: String, args: Any*) = synchronized { evalD0(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL0]].  */
  def invokeL0(function: Reference, args: Any*) = synchronized { evalL0(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL0]].  */
  def invokeL0(functionName: String, args: Any*) = synchronized { evalL0(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS0]].  */
  def invokeS0(function: Reference, args: Any*) = synchronized { evalS0(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS0]].  */
  def invokeS0(functionName: String, args: Any*) = synchronized { evalS0(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR0]].  */
  def invokeR0(function: Reference, args: Any*) = synchronized { evalR0(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR0]].  */
  def invokeR0(functionName: String, args: Any*) = synchronized { evalR0(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getI1]].  */
  def invokeI1(function: Reference, args: Any*) = synchronized { evalI1(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getI1]].  */
  def invokeI1(functionName: String, args: Any*) = synchronized { evalI1(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getD1]].  */
  def invokeD1(function: Reference, args: Any*) = synchronized { evalD1(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getD1]].  */
  def invokeD1(functionName: String, args: Any*) = synchronized { evalD1(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL1]].  */
  def invokeL1(function: Reference, args: Any*) = synchronized { evalL1(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL1]].  */
  def invokeL1(functionName: String, args: Any*) = synchronized { evalL1(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS1]].  */
  def invokeS1(function: Reference, args: Any*) = synchronized { evalS1(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS1]].  */
  def invokeS1(functionName: String, args: Any*) = synchronized { evalS1(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR1]].  */
  def invokeR1(function: Reference, args: Any*) = synchronized { evalR1(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR1]].  */
  def invokeR1(functionName: String, args: Any*) = synchronized { evalR1(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getI2]].  */
  def invokeI2(function: Reference, args: Any*) = synchronized { evalI2(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getI2]].  */
  def invokeI2(functionName: String, args: Any*) = synchronized { evalI2(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getD2]].  */
  def invokeD2(function: Reference, args: Any*) = synchronized { evalD2(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getD2]].  */
  def invokeD2(functionName: String, args: Any*) = synchronized { evalD2(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL2]].  */
  def invokeL2(function: Reference, args: Any*) = synchronized { evalL2(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getL2]].  */
  def invokeL2(functionName: String, args: Any*) = synchronized { evalL2(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS2]].  */
  def invokeS2(function: Reference, args: Any*) = synchronized { evalS2(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getS2]].  */
  def invokeS2(functionName: String, args: Any*) = synchronized { evalS2(mkSnippet(functionName,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR2]].  */
  def invokeR2(function: Reference, args: Any*) = synchronized { evalR2(mkSnippet(function,args)) }
  
  /** Invokes an R function with arguments and returns the result using [[getR2]].  */
  def invokeR2(functionName: String, args: Any*) = synchronized { evalR2(mkSnippet(functionName,args)) }
  
  private def mkSnippet(functionName: String, args: Seq[Any]): String = mkSnippet(EphemeralReference(functionName), args)

  private def mkSnippet(function: Reference, args: Seq[Any]): String = {
    var counter = 0
    val argsStrings = args.map {
      case null => "NULL"
      case (name: String, r: PersistentReference) =>  s"$name = get('$r',envir=.rsI[['r']])"
      case (name: String, r: EphemeralReference) =>  s"$name = $r"
      case (name: String, o) =>
        val id = ".rsX" + counter
        counter += 1
        set(id,o)
        name + "=" + id
      case r: PersistentReference => s"get('$r',envir=.rsI[['r']])"
      case r: EphemeralReference => r.toString
      case o =>
        val id = ".rsX" + counter
        counter += 1
        set(id,o)
        id
    }
    val functionString = function match {
      case r: PersistentReference => s"get('$r',envir=.rsI[['r']])"
      case _ => function.toString
    }
    val snippet = functionString + "(" + argsStrings.mkString(",") +")"
    if ( debug ) debugger.msg("Constructed R snippet: "+snippet)
    snippet
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
  def set(identifier: String, value: Any, index: String = "", singleBrackets: Boolean = true): Unit = synchronized {
    check4GC()
    if ( debug ) debugger.msg("Setting: "+identifier)
    val v = value
    if ( index == "" ) buffer.putInt(SET)
    else if ( singleBrackets ) {
      buffer.putInt(SET_SINGLE)
      writeString(buffer,index)
    } else {
      buffer.putInt(SET_DOUBLE)
      writeString(buffer,index)
    }
    writeString(buffer,identifier)
    var errorNote: Option[String] = None
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debug ) debugger.msg("... which is null")
      buffer.putInt(NULLTYPE)
    } else {
      val c = v.getClass
      if ( debug ) debugger.msg("... whose class is: "+c)
      if ( debug ) debugger.msg("... and whose value is: "+v)
      if ( c.isArray ) {
        c.getName match {
          case "[I" =>
            val vv = v.asInstanceOf[Array[Int]]
            buffer.putInt(VECTOR)
            buffer.putInt(vv.length)
            buffer.putInt(INTEGER)
            for ( i <- 0 until vv.length ) buffer.putInt(vv(i))
          case "[D" =>
            val vv = v.asInstanceOf[Array[Double]]
            buffer.putInt(VECTOR)
            buffer.putInt(vv.length)
            buffer.putInt(DOUBLE)
            for ( i <- 0 until vv.length ) buffer.putDouble(vv(i))
          case "[Z" =>
            val vv = v.asInstanceOf[Array[Boolean]]
            buffer.putInt(VECTOR)
            buffer.putInt(vv.length)
            buffer.putInt(BOOLEAN)
            for ( i <- 0 until vv.length ) buffer.putInt(if ( vv(i) ) 1 else 0)
          case "[Ljava.lang.String;" =>
            val vv = v.asInstanceOf[Array[String]]
            buffer.putInt(VECTOR)
            buffer.putInt(vv.length)
            buffer.putInt(STRING)
            for ( i <- 0 until vv.length ) writeString(buffer,vv(i))
          case "[B" =>
            val vv = v.asInstanceOf[Array[Byte]]
            buffer.putInt(VECTOR)
            buffer.putInt(vv.length)
            buffer.putInt(BYTE)
            buffer.put(vv)
          case "[[I" =>
            val vv1 = v.asInstanceOf[Array[Array[Int]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIfNot(vv1, rowMajor)
              buffer.putInt(MATRIX)
              buffer.putInt(vv.length)
              if ( vv.length > 0 ) buffer.putInt(vv(0).length)
              else buffer.putInt(0)
              buffer.putInt(INTEGER)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vvv.length ) {
                  buffer.putInt(vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              buffer.putInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[D" =>
            val vv1 = v.asInstanceOf[Array[Array[Double]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIfNot(vv1, rowMajor)
              buffer.putInt(MATRIX)
              buffer.putInt(vv.length)
              if ( vv.length > 0 ) buffer.putInt(vv(0).length)
              else buffer.putInt(0)
              buffer.putInt(DOUBLE)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vvv.length ) {
                  buffer.putDouble(vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              buffer.putInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[Z" =>
            val vv1 = v.asInstanceOf[Array[Array[Boolean]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIfNot(vv1, rowMajor)
              buffer.putInt(MATRIX)
              buffer.putInt(vv.length)
              if ( vv.length > 0 ) buffer.putInt(vv(0).length)
              else buffer.putInt(0)
              buffer.putInt(BOOLEAN)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vv(i).length ) {
                  buffer.putInt(if ( vvv(j) ) 1 else 0)
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              buffer.putInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[Ljava.lang.String;" =>
            val vv1 = v.asInstanceOf[Array[Array[String]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIfNot(vv1, rowMajor)
              buffer.putInt(MATRIX)
              buffer.putInt(vv.length)
              if ( vv.length > 0 ) buffer.putInt(vv(0).length)
              else buffer.putInt(0)
              buffer.putInt(STRING)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                for ( j <- 0 until vv(i).length ) {
                  writeString(buffer,vvv(j))
                }
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              buffer.putInt(UNSUPPORTED_STRUCTURE)
            }
          case "[[B" =>
            val vv1 = v.asInstanceOf[Array[Array[Byte]]]
            if ( isMatrix(vv1) ) {
              val vv = transposeIfNot(vv1, rowMajor)
              buffer.putInt(MATRIX)
              buffer.putInt(vv.length)
              if ( vv.length > 0 ) buffer.putInt(vv(0).length)
              else buffer.putInt(0)
              buffer.putInt(BYTE)
              for ( i <- 0 until vv.length ) {
                val vvv = vv(i)
                buffer.put(vvv)
              }
            } else {
              errorNote = Some("Ragged arrays are not supported.")
              buffer.putInt(UNSUPPORTED_STRUCTURE)
            }
          case _ =>
            errorNote = Some("Unsupported array type.")
            buffer.putInt(UNSUPPORTED_STRUCTURE)
        }
      } else {
        c.getName match {
          case "java.lang.Integer" =>
            buffer.putInt(SCALAR)
            buffer.putInt(INTEGER)
            buffer.putInt(v.asInstanceOf[Int])
          case "java.lang.Double" =>
            buffer.putInt(SCALAR)
            buffer.putInt(DOUBLE)
            buffer.putDouble(v.asInstanceOf[Double])
          case "java.lang.Boolean" =>
            buffer.putInt(SCALAR)
            buffer.putInt(BOOLEAN)
            buffer.putInt(if (v.asInstanceOf[Boolean]) 1 else 0)
          case "java.lang.String" =>
            buffer.putInt(SCALAR)
            buffer.putInt(STRING)
            writeString(buffer,v.asInstanceOf[String])
          case "java.lang.Byte" =>
            buffer.putInt(SCALAR)
            buffer.putInt(BYTE)
            buffer.put(v.asInstanceOf[Byte])
          case _ =>
            errorNote = Some("Unsupported non-array type.")
            buffer.putInt(UNSUPPORTED_STRUCTURE)
        }
      }
    }
    buffer.flip()
    sc.write(buffer)
    if ( errorNote.isDefined ) {
      throw new RuntimeException(errorNote.get)
    }
    if ( index != "" ) {
      val status = buffer.getInt()
      if ( status != OK ) {
        val output = readString(sc,buffer)
        if ( output != "" ) println(output)
        throw new RuntimeException("Error in R evaluation.")
      }
    }
  }

  /** __For rscala developers only__:  Returns a value previously cached for the R interpreter. */
  def cached(identifier: String): Any = synchronized {
    if ( identifier.startsWith(".") ) {
      scalaServer.cacheMap(identifier)._1
    } else {
      scalaServer.repl.valueOfTerm(identifier).getOrElse(null)
    }
  }

  /** Returns the value of `identifier` in the R interpreter.  The static type of the result is `(Any,String)`, where
  * the first element is the value and the second is the runtime type.
  *
  * Conversion to integers, doubles, Booleans, and strings are supported, as are vectors (i.e.
  * arrays) and matrices (i.e. retangular arrays of arrays) of these types.    Using the method `getXY` (where `X` is
  * `I`, `D`, `B`, or `S` and `Y` is `0`, `1`, or `2`) may be more convenient (e.g.  [[getD0]]).
  */
  def get(identifier: String, asReference: Boolean = false): (Any, String) = synchronized {
    check4GC()
    if ( debug ) debugger.msg("Getting: "+identifier)
    if ( asReference ) buffer.putInt(GET_REFERENCE) else buffer.putInt(GET)
    writeString(buffer,identifier)
    buffer.flip()
    sc.write(buffer)
    getInternal()
  }

  private def getInternal(): (Any, String) = {
    if ( debug ) debugger.msg("Getting internal.")
    buffer.getInt match {
      case NULLTYPE =>
        if ( debug ) debugger.msg("Getting null.")
        (null,"Null")
      case SCALAR =>
        if ( debug ) debugger.msg("Getting atomic.")
        buffer.getInt() match {
          case INTEGER => (buffer.getInt(),"Int")
          case DOUBLE => (buffer.getDouble(),"Double")
          case BOOLEAN => (( buffer.getInt() != 0 ),"Boolean")
          case STRING => (readString(sc,buffer),"String")
          case BYTE => (buffer.get(),"Byte")
          case _ => throw new RuntimeException("Protocol error")
        }
      case VECTOR =>
        if ( debug ) debugger.msg("Getting vector...")
        val length = buffer.getInt()
        if ( debug ) debugger.msg("... of length: "+length)
        buffer.getInt() match {
          case INTEGER => (Array.fill(length) { buffer.getInt() },"Array[Int]")
          case DOUBLE => (Array.fill(length) { buffer.getDouble() },"Array[Double]")
          case BOOLEAN => (Array.fill(length) { ( buffer.getInt() != 0 ) },"Array[Boolean]")
          case STRING => (Array.fill(length) { readString(sc,buffer) },"Array[String]")
          case BYTE => ( { val bytes = new Array[Byte](length); buffer.get(bytes); bytes },"Array[Byte]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case MATRIX =>
        if ( debug ) debugger.msg("Getting matrix...")
        val nrow = buffer.getInt()
        val ncol = buffer.getInt()
        if ( debug ) debugger.msg("... of dimensions: "+nrow+","+ncol)
        buffer.getInt() match {
          case INTEGER => ( transposeIfNot(Array.fill(nrow) { Array.fill(ncol) { buffer.getInt() } }, rowMajor),"Array[Array[Int]]")
          case DOUBLE => ( transposeIfNot(Array.fill(nrow) { Array.fill(ncol) { buffer.getDouble() } }, rowMajor),"Array[Array[Double]]")
          case BOOLEAN => ( transposeIfNot(Array.fill(nrow) { Array.fill(ncol) { ( buffer.getInt() != 0 ) } }, rowMajor),"Array[Array[Boolean]]")
          case STRING => ( transposeIfNot(Array.fill(nrow) { Array.fill(ncol) { readString(sc,buffer) } }, rowMajor),"Array[Array[String]]")
          case BYTE => ( transposeIfNot(Array.fill(nrow) { val bytes = new Array[Byte](ncol); buffer.get(bytes); bytes }, rowMajor),"Array[Array[Byte]]")
          case _ => throw new RuntimeException("Protocol error")
        }
      case REFERENCE =>
        if ( debug ) debugger.msg("Getting reference.")
        val reference = PersistentReference(readString(sc,buffer))
        val phantomReference = new PhantomReference(reference, referenceQueue)
        referenceMap(phantomReference) = reference.name
        (reference, "org.ddahl.rscala.PersistentReference")
      case UNDEFINED_IDENTIFIER => throw new RuntimeException("Undefined identifier.")
      case UNSUPPORTED_STRUCTURE => throw new RuntimeException("Unsupported data type.")
      case e => throw new RuntimeException("Protocol error: Unknown type: "+e)
    }
  }

  /** Obtains a persistent R reference to the named object so that it can be accessed outside of the current environment. */
  def getReference(identifier: String): PersistentReference = toReference(get(identifier,true))

  /** Converts an ephemeral R reference to a persistent R reference so that it can be accessed outside of the current environment. */
  def getReference(reference: EphemeralReference): PersistentReference = getReference(reference.name)

  private def toReference(x: (Any, String)): PersistentReference = x._1.asInstanceOf[PersistentReference]

  /** Calls '''`get(identifier,false)`''' and converts the result to an `Int`.
  *
  * Integers, doubles, Booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  */
  def getI0(identifier: String): Int = toI0(get(identifier))

  private def toI0(x: (Any, String)): Int = x match {
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
  def getD0(identifier: String): Double = toD0(get(identifier))

  private def toD0(x: (Any, String)): Double = x match {
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
  def getL0(identifier: String): Boolean = toL0(get(identifier))

  private def toL0(x: (Any, String)): Boolean = x match {
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
  def getS0(identifier: String): String = toS0(get(identifier))

  private def toS0(x: (Any, String)): String = x match {
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
  def getR0(identifier: String): Byte = toR0(get(identifier))

  private def toR0(x: (Any, String)): Byte = x match {
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
  def getI1(identifier: String): Array[Int] = toI1(get(identifier))

  private def toI1(x: (Any, String)): Array[Int] = x match {
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
  def getD1(identifier: String): Array[Double] = toD1(get(identifier))

  private def toD1(x: (Any, String)): Array[Double] = x match {
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
  def getL1(identifier: String): Array[Boolean] = toL1(get(identifier))

  private def toL1(x: (Any, String)): Array[Boolean] = x match {
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
  def getS1(identifier: String): Array[String] = toS1(get(identifier))

  private def toS1(x: (Any, String)): Array[String] = x match {
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
  def getR1(identifier: String): Array[Byte] = toR1(get(identifier))

  private def toR1(x: (Any, String)): Array[Byte] = x match {
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
  def getI2(identifier: String): Array[Array[Int]] = toI2(get(identifier))

  private def toI2(x: (Any, String)): Array[Array[Int]] = x match {
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
  def getD2(identifier: String): Array[Array[Double]] = toD2(get(identifier))

  private def toD2(x: (Any, String)): Array[Array[Double]] = x match {
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
  def getL2(identifier: String): Array[Array[Boolean]] = toL2(get(identifier))

  private def toL2(x: (Any, String)): Array[Array[Boolean]] = x match {
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
  def getS2(identifier: String): Array[Array[String]] = toS2(get(identifier))

  private def toS2(x: (Any, String)): Array[Array[String]] = x match {
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
  def getR2(identifier: String): Array[Array[Byte]] = toR2(get(identifier))

  private def toR2(x: (Any, String)): Array[Array[Byte]] = x match {
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
  def apply(rCmd: String = defaultRCmd, serializeOutput: Boolean = true, rowMajor: Boolean = true, port: Int = 0, debug: Boolean = false, timeout: Int = 60): RClient = {
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
    val rProcessInstance = processCmd.run(processIO)
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
    val sockets = new ScalaSockets(portsFile.getAbsolutePath,port,64*1024*1024,debugger)
    sockets.buffer.putInt(OK)
    sockets.buffer.flip()
    sockets.sc.write(sockets.buffer)
    apply(null,rProcessInstance,sockets.sc,sockets.buffer,debugger,serializeOutput,rowMajor)
  }

  /** __For rscala developers only__: Returns an instance of the [[RClient]] class.  */
  def apply(scalaServer: ScalaServer, rProcessInstance: Process, sc: SocketChannel, buffer: ByteBuffer, debugger: Debugger, serializeOutput: Boolean, rowMajor: Boolean): RClient = new RClient(scalaServer,rProcessInstance,sc,buffer,debugger,serializeOutput,rowMajor)

}

