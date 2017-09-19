package org.ddahl.rscala.server

import scala.annotation.tailrec
import scala.tools.nsc.interpreter.{IMain,ILoop}
import scala.tools.nsc.interpreter.IR.Success
import scala.tools.nsc.Settings
import java.net._
import java.io._
import java.nio.ByteBuffer

import org.ddahl.rscala._
import Protocol._
 
class ScalaServer private (private[rscala] val repl: IMain, pw: PrintWriter, baosOut: ByteArrayOutputStream, baosErr: ByteArrayOutputStream, portsFilename: String, debugger: Debugger, serializeOutput: Boolean, rowMajor: Boolean, port: Int, bufferSize: Int) {

  private val sockets = new ScalaSockets(portsFilename,port,bufferSize,debugger)

  import sockets.{sc, buffer, inFill, outFill, bytesPerInt, bytesPerDouble, readString, writeString}
  import Helper.{isMatrix, transposeIfNot}

  buffer.putInt(OK)
  buffer.flip()
  sc.write(buffer)
  private val R = RClient(this,null,sockets,debugger,serializeOutput,rowMajor)
  if ( repl.bind("R","org.ddahl.rscala.RClient",R) != Success ) sys.error("Problem binding R.")
  if ( repl.interpret("import org.ddahl.rscala.{EphemeralReference, PersistentReference}") != Success ) sys.error("Problem interpreting import statement.")
  if ( serializeOutput ) {
    baosOut.reset
    baosErr.reset
  }

  private[rscala] val cacheMap = new Cache()

  private var functionResult: (Any, String) = null
  private val nullary = Class.forName("scala.Function0").getMethod("apply")
  nullary.setAccessible(true)
  private val functionMap = new scala.collection.mutable.HashMap[String,(Any,String)]()

  private def typeOfTerm(id: String) = repl.symbolOfLine(id).info.toString

  private def mostRecentVar = {
    val mrv = repl.mostRecentVar
    if ( mrv == "_rsMRVDummy" ) throw new RuntimeException("There is no result.  Maybe use the %@% operator instead?")
    mrv
  }

  private def extractReturnType(signature: String) = {
    var squareCount = 0
    var parenCount = 0
    var i = 0
    val s = signature.trim
    while ( ( ( parenCount > 0 ) || ( squareCount > 0 ) || ( ! ( ( s(i) == '=' ) && ( s(i+1) == '>' ) ) ) ) && ( i < s.length ) ) {
      val c = s(i)
      i += 1
      if ( c == '(' ) parenCount += 1
      if ( c == ')' ) parenCount -= 1
      if ( c == '[' ) squareCount += 1
      if ( c == ']' ) squareCount -= 1
    }
    if ( i == s.length ) throw new IllegalArgumentException("Unexpected end of signature."+signature)
    i += 2
    val r = s.substring(i).trim
    if ( r.length == 0 ) throw new IllegalArgumentException("Unexpected end of signature."+signature)
    r
  }

  private def writeString(string: String): Unit = {
    if ( debugger.value ) debugger.msg("Writing string: <"+string+">")
    Helper.writeString(buffer,string)
  }

  private def setAVM(identifier: String, t: String, v: Any): Unit = {
    if ( debugger.value ) debugger.msg("Value is "+v)
    repl.bind(identifier,t,v)
  }

  private val sep = sys.props("line.separator")

  private def doFree(): Unit = {
    val nItems = buffer.getInt()
    if ( debugger.value ) debugger.msg("Freeing "+nItems+" cached items no longer needed by R interpreter.")
    for ( i <- 0 until nItems ) cacheMap.free(readString())
  }

  private def doEval(): Unit = {
    val snippet = "var _rsMRVDummy = null\n" + readString()
    try {
      val result = repl.interpret(snippet)
      if ( result == Success ) {
        if ( debugger.value ) debugger.msg("Eval is okay.")
        R.exit()
        buffer.putInt(OK)
      } else {
        if ( debugger.value ) debugger.msg("Eval had a parse error.")
        R.exit()
        buffer.putInt(ERROR)
      }
    } catch {
      case e: Throwable =>
        if ( debugger.value ) debugger.msg("Caught throwable: "+e.toString)
        R.exit()
        buffer.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doDef(): Unit = {
    try {
      val functionName = readString()
      val functionReturnType = readString()
      functionMap(functionName) = (repl.valueOfTerm(functionName).get, functionReturnType)
      buffer.putInt(OK)
    } catch {
      case e: Throwable =>
        buffer.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doInvoke(): Unit = {
    try {
      val functionName = readString()
      val (f, returnType) = functionMap(functionName)
      if ( debugger.value ) {
        debugger.msg("Function is: "+functionName)
        debugger.msg("... with return type: "+returnType)
      }
      functionResult = (nullary.invoke(f), returnType)
      R.exit()
      if ( debugger.value ) debugger.msg("Invoke is okay")
      buffer.putInt(OK)
    } catch {
      case e: Throwable =>
        R.exit()
        buffer.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private val RegexpVar = """^\s+(?:final )?\s*(?:override )?\s*var\s+([^(:]+):\s+([^=;]+).*""".r
  private val RegexpVal = """^\s+(?:final )?\s*(?:override )?\s*val\s+([^(:]+):\s+([^=;]+).*""".r
  private val RegexpDefNoParen = """^\s+(?:final )?\s*(?:override)?\s*def\s+([a-zA-Z0-9]+)(?:\[.*\]:|:)\s+([^=;]+).*""".r
  private val RegexpDefWithParen = """^\s+(?:final )?\s*(?:override)?\s*def\s+([a-zA-Z0-9]+)(?:\[.*\]+)?\((.*)\):\s+([^=]+).*""".r
  private val RegexpConstructor = """^\s+def\s+this\((.*)\).*""".r
  private val RegexpThrows = """^\s*throws\s+.*""".r
  private val rscalaReference = "rscalaReference"

  private def parseArgNames(args: String): Array[String] = {
    if ( args.contains(")(") ) null
    else {
      val x = args.split(":")
      val y = if ( x.length > 1 ) x.dropRight(1) else x
      y.map(z => {
        val i = z.lastIndexOf(",")
        if ( i >= 0 ) z.substring(i+1).trim else z.trim
      })
    }
  }

  private def doScalap(): Unit = {
    if ( debugger.value ) debugger.msg("Doing scalap...")
    val itemName = readString()
    if ( debugger.value ) debugger.msg("... on "+itemName)
    val classpath = sys.props("sun.boot.class.path") + sys.props("path.separator") + sys.props("rscala.classpath")
    if ( debugger.value ) debugger.msg("... with classpath "+classpath)
    scala.tools.scalap.Main.main(Array("-cp",classpath,itemName))
    buffer.putInt(OK)
  }

  private def doSet(): Unit = {
    val identifier = readString()
    inFill(bytesPerInt)
    buffer.getInt() match {
      case NULLTYPE =>
        if ( debugger.value ) debugger.msg("Setting null.")
        repl.bind(identifier,"Any",null)
      case REFERENCE =>
        if ( debugger.value ) debugger.msg("Setting reference.")
        val originalIdentifier = readString()
        try {
          val (value,tipe) = originalIdentifier match {
            case cacheMap(value,typeOfTerm) => (value,typeOfTerm)
            case _ => (repl.valueOfTerm(originalIdentifier).get,typeOfTerm(originalIdentifier))
          }
          if ( repl.bind(identifier,tipe,value) != Success ) throw new RuntimeException("Cannot set reference.")
          buffer.putInt(OK)
        } catch {
          case e: Throwable =>
            if ( debugger.value ) debugger.msg("Caught exception: "+e)
            buffer.putInt(ERROR)
            e.printStackTrace(pw)
            pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
        }
      case SCALAR =>
        if ( debugger.value ) debugger.msg("Setting atomic.")
        inFill(bytesPerInt)
        val (v: Any, t: String) = buffer.getInt() match {
          case INTEGER =>
            inFill(bytesPerInt)
            (buffer.getInt(),"Int")
          case DOUBLE =>
            inFill(bytesPerDouble)
            (buffer.getDouble(),"Double")
          case BOOLEAN =>
            inFill(bytesPerInt)
            (( buffer.getInt() != 0 ),"Boolean")
          case STRING => (readString(),"String")
          case BYTE =>
            inFill(1)
            (buffer.get(),"Byte")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case VECTOR =>
        if ( debugger.value ) debugger.msg("Setting vector...")
        inFill(2*bytesPerInt)
        val length = buffer.getInt()
        if ( debugger.value ) debugger.msg("... of length: "+length)
        val (v, t): (Any,String) = buffer.getInt() match {
          case INTEGER =>
            inFill(length*bytesPerInt)
            val array = new Array[Int](length)
            buffer.asIntBuffer.get(array)
            (array,"Array[Int]")
          case DOUBLE =>
            inFill(length*bytesPerDouble)
            val array = new Array[Double](length)
            buffer.asDoubleBuffer.get(array)
            (array,"Array[Double]")
          case BOOLEAN =>
            inFill(length*bytesPerInt)
            (Array.fill(length) { ( buffer.getInt() != 0 ) },"Array[Boolean]")
          case STRING => (Array.fill(length) { readString() },"Array[String]")
          case BYTE =>
            val buffer2 = ByteBuffer.allocate(length)
            sc.read(buffer2)
            (buffer2.array,"Array[Byte]")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case MATRIX =>
        if ( debugger.value ) debugger.msg("Setting matrix...")
        inFill(3*bytesPerInt)
        val nrow = buffer.getInt()
        val ncol = buffer.getInt()
        if ( debugger.value ) debugger.msg("... of dimensions: "+nrow+","+ncol)
        val (v, t): (Any,String) = buffer.getInt() match {
          case INTEGER =>
            inFill(nrow*ncol*bytesPerInt)
            val array = Array.fill(nrow) {
              val array = new Array[Int](ncol)
              buffer.asIntBuffer.get(array)
              array
            }
            (transposeIfNot(array,rowMajor),"Array[Array[Int]]")
          case DOUBLE =>
            inFill(nrow*ncol*bytesPerDouble)
            val array = Array.fill(nrow) {
              val array = new Array[Double](ncol)
              buffer.asDoubleBuffer.get(array)
              array
            }
            ( transposeIfNot(array, rowMajor),"Array[Array[Double]]")
          case BOOLEAN =>
            inFill(nrow*ncol*bytesPerInt)
            val array =  Array.fill(nrow) { Array.fill(ncol) { ( buffer.getInt() != 0 ) } }
            ( transposeIfNot(array, rowMajor),"Array[Array[Boolean]]")
          case STRING => ( transposeIfNot( Array.fill(nrow) { Array.fill(ncol) { readString() } }, rowMajor),"Array[Array[String]]")
          case BYTE =>
            inFill(nrow*ncol)
            ( transposeIfNot( Array.fill(nrow) {
              val buffer2 = ByteBuffer.allocate(ncol)
              sc.read(buffer2)
              buffer2.array
            }, rowMajor),"Array[Array[Byte]]")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case _ => throw new RuntimeException("Protocol error")
    }
  }

  private def doGet(): Unit = {
    val identifier = readString()
    if ( debugger.value ) debugger.msg("Trying to get value of: "+identifier)
    val optionWithType = try {
      identifier match {
        case "." => 
          val mrv = mostRecentVar
          (repl.valueOfTerm(mrv),typeOfTerm(mrv))
        case "?" => 
          (Some(functionResult._1),functionResult._2)
        case cacheMap(value,typeOfTerm) =>
          (Some(value),typeOfTerm)
        case "null" =>
          (Some(null),"Null")
        case _ =>
          (repl.valueOfTerm(identifier),typeOfTerm(identifier))
      }
    } catch {
      case e: Throwable =>
        if ( debugger.value ) debugger.msg("Caught exception: "+e)
        buffer.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
        return
    }
    if ( debugger.value ) debugger.msg("Getting: "+identifier)
    if ( optionWithType._1.isEmpty ) {
      if ( optionWithType._2 == "<notype>" ) {
        if ( debugger.value ) debugger.msg("... which does not exist.")
        buffer.putInt(UNDEFINED_IDENTIFIER)
        return
      } else {
        if ( debugger.value ) debugger.msg("... which is emtpy [due to quirk in 2.10 series].")
        buffer.putInt(NULLTYPE)
        return
      }
    }
    val v = optionWithType._1.get
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debugger.value ) debugger.msg("... which is null")
      buffer.putInt(NULLTYPE)
      return
    }
    val t = if ( optionWithType._2 == "Any" ) {
      val c = v.getClass
      if ( debugger.value ) debugger.msg("Trying to infer type of "+c)
      if ( c.isArray ) {
        c.getName match {
          case "[I" => "Array[Int]"
          case "[D" => "Array[Double]"
          case "[Z" => "Array[Boolean]"
          case "[Ljava.lang.String;" => "Array[String]"
          case "[B" => "Array[Byte]"
          case "[[I" => "Array[Array[Int]]"
          case "[[D" => "Array[Array[Double]]"
          case "[[Z" => "Array[Array[Boolean]]"
          case "[[Ljava.lang.String;" => "Array[Array[String]]"
          case "[[B" => "Array[Array[Byte]]"
          case _ => "Any"
        }
      } else {
        c.getName match {
          case "java.lang.Integer" => "Int"
          case "java.lang.Double" => "Double"
          case "java.lang.Boolean" => "Boolean"
          case "java.lang.String" => "String"
          case "java.lang.Byte" => "Byte"
          case _ => "Any"
        }
      }
    } else optionWithType._2
    if ( debugger.value ) debugger.msg("... whose type is: "+t)
    if ( debugger.value ) debugger.msg("... and whose value is: "+v)
    t match {
      case "Array[Int]" =>
        val vv = v.asInstanceOf[Array[Int]]
        buffer.putInt(VECTOR)
        buffer.putInt(vv.length)
        buffer.putInt(INTEGER)
        for ( i <- 0 until vv.length ) buffer.putInt(vv(i))
      case "Array[Double]" =>
        val vv = v.asInstanceOf[Array[Double]]
        buffer.putInt(VECTOR)
        buffer.putInt(vv.length)
        buffer.putInt(DOUBLE)
        for ( i <- 0 until vv.length ) buffer.putDouble(vv(i))
      case "Array[Boolean]" =>
        val vv = v.asInstanceOf[Array[Boolean]]
        buffer.putInt(VECTOR)
        buffer.putInt(vv.length)
        buffer.putInt(BOOLEAN)
        for ( i <- 0 until vv.length ) buffer.putInt(if ( vv(i) ) 1 else 0)
      case "Array[String]" =>
        val vv = v.asInstanceOf[Array[String]]
        buffer.putInt(VECTOR)
        buffer.putInt(vv.length)
        buffer.putInt(STRING)
        for ( i <- 0 until vv.length ) writeString(vv(i))
      case "Array[Byte]" =>
        val vv = v.asInstanceOf[Array[Byte]]
        buffer.putInt(VECTOR)
        buffer.putInt(vv.length)
        buffer.putInt(BYTE)
        buffer.put(vv)
      case "Array[Array[Int]]" =>
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
        } else buffer.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Double]]" =>
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
        } else buffer.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Boolean]]" =>
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
        } else buffer.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[String]]" =>
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
              writeString(vvv(j))
            }
          }
        } else buffer.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Byte]]" =>
        val vv1 = v.asInstanceOf[Array[Array[Byte]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          buffer.putInt(MATRIX)
          buffer.putInt(vv.length)
          if ( vv.length > 0 ) buffer.putInt(vv(0).length)
          else buffer.putInt(0)
          buffer.putInt(BYTE)
          for ( i <- 0 until vv.length ) {
            buffer.put(vv(i))
          }
        } else buffer.putInt(UNSUPPORTED_STRUCTURE)
      case "Int" =>
        buffer.putInt(SCALAR)
        buffer.putInt(INTEGER)
        buffer.putInt(v.asInstanceOf[Int])
      case "Double" =>
        buffer.putInt(SCALAR)
        buffer.putInt(DOUBLE)
        buffer.putDouble(v.asInstanceOf[Double])
      case "Boolean" =>
        buffer.putInt(SCALAR)
        buffer.putInt(BOOLEAN)
        buffer.putInt(if (v.asInstanceOf[Boolean]) 1 else 0)
      case "String" =>
        buffer.putInt(SCALAR)
        buffer.putInt(STRING)
        writeString(v.asInstanceOf[String])
      case "Byte" =>
        buffer.putInt(SCALAR)
        buffer.putInt(BYTE)
        buffer.put(v.asInstanceOf[Byte])
      case _ =>
        if ( debugger.value ) debugger.msg("Bowing out: "+identifier)
        buffer.putInt(UNSUPPORTED_STRUCTURE)
    }
  }

  private def doGetReference(): Unit = {
    val identifier = readString()
    if ( debugger.value ) debugger.msg("Trying to get reference for: "+identifier)
    identifier match {
      case "?" =>
        buffer.putInt(OK)
        writeString(cacheMap.store(functionResult))
        writeString(functionResult._2)
      case cacheMap(value, typeOfTerm) =>
        buffer.putInt(OK)
        writeString(identifier)
        writeString(typeOfTerm)
      case _ =>
        if ( identifier == "null" ) {
          buffer.putInt(OK)
          writeString("null")
          writeString("Null")
          return
        }
        val (id, opt) = try {
          val ident = if ( identifier == "." ) mostRecentVar else identifier
          val option = repl.valueOfTerm(ident)
          (ident, option)
        } catch {
          case e: Throwable =>
            if ( debugger.value ) debugger.msg("Caught exception: "+e)
            buffer.putInt(ERROR)
            e.printStackTrace(pw)
            pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
            return
        }
        if ( opt.isEmpty ) buffer.putInt(UNDEFINED_IDENTIFIER)
        else {
          buffer.putInt(OK)
          writeString(id)
          writeString(typeOfTerm(id))
        }
    }
  }

  private def heart(request: Int): Boolean = {
    if ( debugger.value ) debugger.msg("Received request: "+request)
    request match {
      case SHUTDOWN =>
        if ( debugger.value ) debugger.msg("Shutting down")
        sockets.sc.close()
        return false
      case EXIT =>
        if ( debugger.value ) debugger.msg("Exiting")
        return false
      case FREE =>
        doFree()
      case EVAL =>
        doEval()
      case SET =>
        doSet()
      case GET =>
        doGet()
      case GET_REFERENCE =>
        doGetReference()
      case DEF =>
        doDef()
      case INVOKE =>
        doInvoke()
      case SCALAP =>
        doScalap()
    }
    true
  }

  final def run(): Unit = {
    while ( true ) {
      if ( debugger.value ) debugger.msg("Scala server at top of the loop waiting for a command.")
      buffer.clear()
      buffer.limit(bytesPerInt)
      sc.read(buffer)
      val request = try {
        buffer.getInt()
      } catch {
        case _: Throwable =>
          return
      }
      if ( serializeOutput ) {
        scala.Console.withOut(baosOut) {
          scala.Console.withErr(baosErr) {
            if ( ! heart(request) ) return
          }
        }
        pw.flush()
        writeString(baosOut.toString+baosErr.toString)
        baosOut.reset()
        baosErr.reset()
      } else {
        if ( ! heart(request) ) return
      }
      buffer.flip()
      sc.write(buffer)
    }
  }

}

object ScalaServer {

  def apply(portsFilename: String, debug: Boolean = false, serializeOutput: Boolean = false, rowMajor: Boolean = true, port: Int = 0): ScalaServer = {
    // Set classpath
    val settings = new Settings()
    settings.embeddedDefaults[RClient]
    val urls = java.lang.Thread.currentThread.getContextClassLoader match {
      case cl: java.net.URLClassLoader => cl.getURLs.toList
      case _ => sys.error("Classloader is not a URLClassLoader")
    }
    val classpath = urls.map(_.getPath)
    settings.classpath.value = classpath.distinct.mkString(java.io.File.pathSeparator)
    settings.deprecation.value = true
    settings.feature.value = true
    settings.unchecked.value = true
    // Allow reflective calls without a warning since we make us of them.
    // Use refective since Scala 2.10.x doess not have the settings.language.add method.
    val m1 = if ( util.Properties.versionNumberString.split("""\.""").take(2).mkString(".") == "2.10" ) {
      settings.language.getClass.getMethod("appendToValue","".getClass)
    } else {
      settings.language.getClass.getMethod("add","".getClass)
    }
    m1.setAccessible(true)
    m1.invoke(settings.language,"reflectiveCalls")
    // A better way to do it, but Scala 2.10.x doess not the settings.language.add method.
    // settings.language.add("reflectiveCalls")
    // Set up sinks
    val (debugger,prntWrtr,baosOut,baosErr) = serializeOutput match {
      case true =>
        val out = new ByteArrayOutputStream()
        val err = new ByteArrayOutputStream()
        val pw = new PrintWriter(out)
        val d = new Debugger(debug,pw,"Scala",false)
        (d,pw,out,err)
      case false =>
        val pw = new PrintWriter(System.out,true)
        val d = new Debugger(debug,pw,"Scala",false)
        (d,pw,null,null)
    }
    if ( debugger.value ) debugger.msg("Classpath is:\n"+classpath.mkString("\n"))
    // Instantiate an interpreter
    val intp = new IMain(settings,prntWrtr)
    // Suppress output; equivalent to :silent in REPL, but it's private, so use reflection.
    val m2 = intp.getClass.getMethod("printResults_$eq",java.lang.Boolean.TYPE)
    m2.setAccessible(true)
    m2.invoke(intp,java.lang.Boolean.FALSE)
    // Another way to do it, but Scala 2.10.x is chatty and prints "Switched off result printing."
    // val iloop = new ILoop()
    // iloop.intp = intp
    // iloop.verbosity()
    // Return the server
    new ScalaServer(intp, prntWrtr, baosOut, baosErr, portsFilename, debugger, serializeOutput, rowMajor, port, 64*1024*1024)
  }

}

