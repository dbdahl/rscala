package org.ddahl.rscala.server

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.IR.Success
import scala.tools.nsc.Settings
import java.io._
import java.nio.ByteBuffer

import org.ddahl.rscala._
import Protocol._
 
class ScalaServer private (private[rscala] val repl: IMain, pw: PrintWriter, baosOut: ByteArrayOutputStream, baosErr: ByteArrayOutputStream, portsFilename: String, debugger: Debugger, serializeOutput: Boolean, rowMajor: Boolean, port: Int, bufferSize: Int) {

  private val socket = new ScalaSocket(portsFilename,port,bufferSize,debugger)

  // import socket.{buffer, inFill, outFill, bytesPerInt, bytesPerDouble, readString, writeString, putInt, getInt, flush}
  import Helper.{isMatrix, transposeIfNot}

  socket.putInt(OK)
  socket.flush()
  private val R = RClient(this,null,socket,debugger,serializeOutput,rowMajor)
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

  private def setAVM(identifier: String, t: String, v: Any): Unit = {
    if ( debugger.value ) debugger.msg("Value is "+v)
    repl.bind(identifier,t,v)
  }

  private def doFree(): Unit = {
    val nItems = socket.getInt()
    if ( debugger.value ) debugger.msg("Freeing "+nItems+" cached items no longer needed by R interpreter.")
    for ( i <- 0 until nItems ) cacheMap.free(socket.getScalarString())
  }

  private def doEval(): Unit = {
    val snippet = "var _rsMRVDummy = null\n" + socket.getScalarString()
    try {
      val result = repl.interpret(snippet)
      if ( result == Success ) {
        if ( debugger.value ) debugger.msg("Eval is okay.")
        R.exit()
        socket.putInt(OK)
      } else {
        if ( debugger.value ) debugger.msg("Eval had a parse error.")
        R.exit()
        socket.putInt(ERROR)
      }
    } catch {
      case e: Throwable =>
        if ( debugger.value ) debugger.msg("Caught throwable: "+e.toString)
        R.exit()
        socket.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doDef(): Unit = {
    try {
      val functionName = socket.getScalarString()
      val functionReturnType = socket.getScalarString()
      functionMap(functionName) = (repl.valueOfTerm(functionName).get, functionReturnType)
      socket.putInt(OK)
    } catch {
      case e: Throwable =>
        socket.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doInvoke(): Unit = {
    try {
      val functionName = socket.getScalarString()
      val (f, returnType) = functionMap(functionName)
      if ( debugger.value ) {
        debugger.msg("Function is: "+functionName)
        debugger.msg("... with return type: "+returnType)
      }
      functionResult = (nullary.invoke(f), returnType)
      R.exit()
      if ( debugger.value ) debugger.msg("Invoke is okay")
      socket.putInt(OK)
    } catch {
      case e: Throwable =>
        R.exit()
        socket.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doScalap(): Unit = {
    if ( debugger.value ) debugger.msg("Doing scalap...")
    val itemName = socket.getScalarString()
    if ( debugger.value ) debugger.msg("... on "+itemName)
    val classpath = sys.props("sun.boot.class.path") + sys.props("path.separator") + sys.props("rscala.classpath")
    if ( debugger.value ) debugger.msg("... with classpath "+classpath)
    scala.tools.scalap.Main.main(Array("-cp",classpath,itemName))
    socket.putInt(OK)
  }

  private def doSet(): Unit = {
    val identifier = socket.getScalarString()
    val shape = socket.getScalarInt()
    shape match {
      case NULLTYPE =>
        if ( debugger.value ) debugger.msg("Setting null.")
        repl.bind(identifier,"Any",null)
      case REFERENCE =>
        if ( debugger.value ) debugger.msg("Setting reference.")
        val originalIdentifier = socket.getScalarString()
        try {
          val (value,tipe) = originalIdentifier match {
            case cacheMap(v,typeOfTerm) => (v,typeOfTerm)
            case _ => (repl.valueOfTerm(originalIdentifier).get,typeOfTerm(originalIdentifier))
          }
          if ( repl.bind(identifier,tipe,value) != Success ) throw new RuntimeException("Cannot set reference.")
          socket.outFill(socket.bytesPerInt)
          socket.putInt(OK)
        } catch {
          case e: Throwable =>
            if ( debugger.value ) debugger.msg("Caught exception: "+e)
            socket.outFill(socket.bytesPerInt)
            socket.putInt(ERROR)
            e.printStackTrace(pw)
            pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
        }
      case SCALAR =>
        if ( debugger.value ) debugger.msg("Setting atomic.")
        val tipe = socket.getScalarInt()
        val (v: Any, t: String) = tipe match {
          case INTEGER => (socket.getScalarInt(),    "Int")
          case DOUBLE =>  (socket.getScalarDouble(), "Double")
          case BOOLEAN => (socket.getScalarBoolean(),"Boolean")
          case STRING =>  (socket.getScalarString(), "String")
          case BYTE =>    (socket.getScalarByte(),   "Byte")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case VECTOR =>
        if ( debugger.value ) debugger.msg("Setting vector...")
        val (length, tipe) = socket.getTuple2Int()
        if ( debugger.value ) debugger.msg("... of length: "+length)
        val (v, t): (Any,String) = tipe match {
          case INTEGER => (socket.getVectorInt(length),    "Array[Int]")
          case DOUBLE =>  (socket.getVectorDouble(length), "Array[Double]")
          case BOOLEAN => (socket.getVectorBoolean(length),"Array[Boolean]")
          case STRING =>  (socket.getVectorString(length), "Array[String]")
          case BYTE =>    (socket.getVectorByte(length),   "Array[Byte]")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case MATRIX =>
        if ( debugger.value ) debugger.msg("Setting matrix...")
        val (nrow, ncol, tipe) = socket.getTuple3Int()
        if ( debugger.value ) debugger.msg("... of dimensions: "+nrow+","+ncol)
        val (v, t): (Any,String) = tipe match {
          case INTEGER =>
            socket.getMatrixInt(nrow,ncol,rowMajor)
            val array = Array.fill(nrow) { socket.getVectorInt(ncol) }
            (transposeIfNot(array,rowMajor),"Array[Array[Int]]")
          case DOUBLE =>
            socket.inFill(nrow*ncol*socket.bytesPerDouble)
            val array = Array.fill(nrow) { socket.getVectorDouble(ncol) }
            ( transposeIfNot(array, rowMajor),"Array[Array[Double]]")
          case BOOLEAN =>
            socket.inFill(nrow*ncol*socket.bytesPerBoolean)
            val array =  Array.fill(nrow) { socket.getVectorBoolean(ncol) }
            ( transposeIfNot(array, rowMajor),"Array[Array[Boolean]]")
          case STRING => ( transposeIfNot( Array.fill(nrow) { Array.fill(ncol) { socket.getScalarString() } }, rowMajor),"Array[Array[String]]")
          case BYTE =>
            socket.inFill(nrow*ncol)
            ( transposeIfNot( Array.fill(nrow) {
              val buffer2 = ByteBuffer.allocate(ncol)
              socket.read(buffer2)
              buffer2.array
            }, rowMajor),"Array[Array[Byte]]")
          case _ => throw new RuntimeException("Protocol error")
        }
        setAVM(identifier,t,v)
      case _ => throw new RuntimeException("Protocol error")
    }
  }

  private def doGet(): Unit = {
    val identifier = socket.getScalarString()
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
        socket.outFill(socket.bytesPerInt)
        socket.putInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
        return
    }
    if ( debugger.value ) debugger.msg("Getting: "+identifier)
    if ( optionWithType._1.isEmpty ) {
      if ( optionWithType._2 == "<notype>" ) {
        if ( debugger.value ) debugger.msg("... which does not exist.")
        socket.outFill(socket.bytesPerInt)
        socket.putInt(UNDEFINED_IDENTIFIER)
        return
      } else {
        if ( debugger.value ) debugger.msg("... which is emtpy [due to quirk in 2.10 series].")
        socket.outFill(socket.bytesPerInt)
        socket.putInt(NULLTYPE)
        return
      }
    }
    val v = optionWithType._1.get
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debugger.value ) debugger.msg("... which is null")
      socket.outFill(socket.bytesPerInt)
      socket.putInt(NULLTYPE)
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
        socket.outFill(3*socket.bytesPerInt + vv.length*socket.bytesPerInt)
        socket.putInt(VECTOR)
        socket.putInt(vv.length)
        socket.putInt(INTEGER)
        socket.putVectorInt(vv)
      case "Array[Double]" =>
        val vv = v.asInstanceOf[Array[Double]]
        socket.outFill(3*socket.bytesPerInt + vv.length*socket.bytesPerDouble)
        socket.putInt(VECTOR)
        socket.putInt(vv.length)
        socket.putInt(DOUBLE)
        socket.putVectorDouble(vv)
      case "Array[Boolean]" =>
        val vv = v.asInstanceOf[Array[Boolean]]
        socket.outFill(3*socket.bytesPerInt + vv.length*socket.bytesPerBoolean)
        socket.putInt(VECTOR)
        socket.putInt(vv.length)
        socket.putInt(BOOLEAN)
        socket.putVectorBoolean(vv)
      case "Array[String]" =>
        val vv = v.asInstanceOf[Array[String]]
        socket.outFill(3*socket.bytesPerInt)
        socket.putInt(VECTOR)
        socket.putInt(vv.length)
        socket.putInt(STRING)
        socket.putVectorString(vv)
      case "Array[Byte]" =>
        val vv = v.asInstanceOf[Array[Byte]]
        socket.outFill(3*socket.bytesPerInt + vv.length)
        socket.putInt(VECTOR)
        socket.putInt(vv.length)
        socket.putInt(BYTE)
        socket.putVectorByte(vv)
      case "Array[Array[Int]]" =>
        val vv1 = v.asInstanceOf[Array[Array[Int]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          socket.putInt(MATRIX)
          socket.putInt(vv.length)
          if ( vv.length > 0 ) socket.putInt(vv(0).length)
          else socket.putInt(0)
          socket.putInt(INTEGER)
          for ( i <- 0 until vv.length ) {
            val vvv = vv(i)
            for ( j <- 0 until vvv.length ) {
              socket.putInt(vvv(j))
            }
          }
        } else socket.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Double]]" =>
        val vv1 = v.asInstanceOf[Array[Array[Double]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          socket.putInt(MATRIX)
          socket.putInt(vv.length)
          if ( vv.length > 0 ) socket.putInt(vv(0).length)
          else socket.putInt(0)
          socket.putInt(DOUBLE)
          for ( i <- 0 until vv.length ) {
            val vvv = vv(i)
            for ( j <- 0 until vvv.length ) {
              buffer.putDouble(vvv(j))
            }
          }
        } else socket.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Boolean]]" =>
        val vv1 = v.asInstanceOf[Array[Array[Boolean]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          socket.putInt(MATRIX)
          socket.putInt(vv.length)
          if ( vv.length > 0 ) socket.putInt(vv(0).length)
          else socket.putInt(0)
          socket.putInt(BOOLEAN)
          for ( i <- 0 until vv.length ) {
            val vvv = vv(i)
            for ( j <- 0 until vv(i).length ) {
              socket.putInt(if ( vvv(j) ) 1 else 0)
            }
          }
        } else socket.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[String]]" =>
        val vv1 = v.asInstanceOf[Array[Array[String]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          socket.putInt(MATRIX)
          socket.putInt(vv.length)
          if ( vv.length > 0 ) socket.putInt(vv(0).length)
          else socket.putInt(0)
          socket.putInt(STRING)
          for ( i <- 0 until vv.length ) {
            val vvv = vv(i)
            for ( j <- 0 until vv(i).length ) {
              socket.putScalarString(vvv(j))
            }
          }
        } else socket.putInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Byte]]" =>
        val vv1 = v.asInstanceOf[Array[Array[Byte]]]
        if ( isMatrix(vv1) ) {
          val vv = transposeIfNot(vv1, rowMajor)
          socket.putInt(MATRIX)
          socket.putInt(vv.length)
          if ( vv.length > 0 ) socket.putInt(vv(0).length)
          else socket.putInt(0)
          socket.putInt(BYTE)
          for ( i <- 0 until vv.length ) {
            buffer.put(vv(i))
          }
        } else socket.putInt(UNSUPPORTED_STRUCTURE)
      case "Int" =>
        socket.outFill(2*socket.bytesPerInt + socket.bytesPerInt)
        socket.putInt(SCALAR)
        socket.putInt(INTEGER)
        socket.putInt(v.asInstanceOf[Int])
      case "Double" =>
        socket.outFill(2*socket.bytesPerInt + socket.bytesPerDouble)
        socket.putInt(SCALAR)
        socket.putInt(DOUBLE)
        socket.putDouble(v.asInstanceOf[Double])
      case "Boolean" =>
        socket.outFill(2*socket.bytesPerInt + socket.bytesPerBoolean)
        socket.putInt(SCALAR)
        socket.putInt(BOOLEAN)
        socket.putInt(if (v.asInstanceOf[Boolean]) 1 else 0)
      case "String" =>
        socket.putInt(SCALAR)
        socket.putInt(STRING)
        socket.putScalarString(v.asInstanceOf[String])
      case "Byte" =>
        socket.putInt(SCALAR)
        socket.putInt(BYTE)
        buffer.put(v.asInstanceOf[Byte])
      case _ =>
        if ( debugger.value ) debugger.msg("Bowing out: "+identifier)
        socket.putInt(UNSUPPORTED_STRUCTURE)
    }
  }

  private def doGetReference(): Unit = {
    val identifier = socket.getScalarString()
    if ( debugger.value ) debugger.msg("Trying to get reference for: "+identifier)
    identifier match {
      case "?" =>
        socket.putInt(OK)
        socket.putScalarString(cacheMap.store(functionResult))
        socket.putScalarString(functionResult._2)
      case cacheMap(value, typeOfTerm) =>
        socket.putInt(OK)
        socket.putScalarString(identifier)
        socket.putScalarString(typeOfTerm)
      case _ =>
        if ( identifier == "null" ) {
          socket.putInt(OK)
          socket.putScalarString("null")
          socket.putScalarString("Null")
          return
        }
        val (id, opt) = try {
          val ident = if ( identifier == "." ) mostRecentVar else identifier
          val option = repl.valueOfTerm(ident)
          (ident, option)
        } catch {
          case e: Throwable =>
            if ( debugger.value ) debugger.msg("Caught exception: "+e)
            socket.putInt(ERROR)
            e.printStackTrace(pw)
            pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
            return
        }
        if ( opt.isEmpty ) socket.putInt(UNDEFINED_IDENTIFIER)
        else {
          socket.putInt(OK)
          socket.putScalarString(id)
          socket.putScalarString(typeOfTerm(id))
        }
    }
  }

  private def heart(request: Int): Boolean = {
    if ( debugger.value ) debugger.msg("Received request: "+request)
    request match {
      case SHUTDOWN =>
        if ( debugger.value ) debugger.msg("Shutting down")
        socket.close()
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
      socket.inFill(socket.bytesPerInt)
      val request = try {
        socket.getInt()
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
        socket.putScalarString(baosOut.toString+baosErr.toString)
        baosOut.reset()
        baosErr.reset()
      } else {
        if ( ! heart(request) ) return
      }
      socket.flush()
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

