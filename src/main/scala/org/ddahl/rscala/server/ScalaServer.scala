package org.ddahl.rscala.server

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.IR.Success
import scala.tools.nsc.Settings
import java.io._
import java.nio.ByteBuffer

import org.ddahl.rscala._
import Protocol._
 
class ScalaServer private (private[rscala] val repl: IMain, pw: PrintWriter, baosOut: ByteArrayOutputStream, baosErr: ByteArrayOutputStream, snippetFilename: String, portsFilename: String, debugger: Debugger, serializeOutput: Boolean, rowMajor: Boolean, port: Int, bufferSize: Int, prioritizeConnect: Boolean) {

  private val R = RClient(this,null,null,debugger,serializeOutput,rowMajor)

  def initialInterpret() {
    if ( repl.bind("R","org.ddahl.rscala.RClient",R) != Success ) sys.error("Problem binding R.  Interpreter is dead.")
    val snippet = try {
      val bufferedSource = scala.io.Source.fromFile(snippetFilename)
      val snip = "import org.ddahl.rscala.{EphemeralReference, PersistentReference}\n" + bufferedSource.getLines.mkString("\n")
      bufferedSource.close
      snip
    } catch {
      case _: Throwable => sys.exit(0)
    }
    if ( repl.interpret(snippet) != Success ) sys.error("Problem interpreting initial snippet.  Interpreter is dead.")
  }

  if ( ! prioritizeConnect ) initialInterpret()

  private val socket = new ScalaSocket(portsFilename,port,bufferSize,debugger)
  socket.putScalarInt(OK)
  socket.flush()

  if ( prioritizeConnect ) initialInterpret()

  R.socket = socket

  if ( serializeOutput ) {
    baosOut.reset
    baosErr.reset
  }

  private[rscala] val cacheMap = new Cache()

  private var functionResult: (Any, String) = null
  private val unary = Class.forName("scala.Function1").getMethod("apply", classOf[java.lang.Object])
  unary.setAccessible(true)
  private val functionMap = new scala.collection.mutable.HashMap[String,(Any,String)]()
  private val functionMap2 = new scala.collection.mutable.HashMap[String,String]()

  private def typeOfTerm(id: String) = repl.symbolOfLine(id).info.toString

  private def mostRecentVar = {
    val mrv = repl.mostRecentVar
    if ( mrv == "_rsMRVDummy" ) throw new RuntimeException("There is no result.  Maybe use the %@% operator instead?")
    mrv
  }

  private def setAVM(identifier: String, t: String, v: Any): Unit = {
    if ( debugger.value ) debugger.msg("Value is "+v)
    repl.bind(identifier,t,v)
  }

  private def doFree(): Unit = {
    val nItems = socket.getScalarInt()
    if ( debugger.value ) debugger.msg("Freeing "+nItems+" cached items no longer needed by R interpreter.")
    for ( i <- 0 until nItems ) cacheMap.free(socket.getScalarString())
  }

  private def doEval(): Unit = {
    val snippet = "var _rsMRVDummy = null\n" + socket.getScalarString()
    try {
      val result = repl.interpret(snippet)
      if ( result == Success ) {
        if ( debugger.value ) debugger.msg("Evaluation is okay.")
        R.exit()
        if ( debugger.value ) debugger.msg("Exited from R.")
        socket.putScalarInt(OK)
      } else {
        if ( debugger.value ) debugger.msg("Eval had a parse error.")
        R.exit()
        socket.putScalarInt(ERROR)
      }
    } catch {
      case e: Throwable =>
        if ( debugger.value ) debugger.msg("Caught throwable: "+e.toString)
        R.exit()
        socket.putScalarInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doDef(): Unit = {
    val body = "($0: String) => {\n" + socket.getScalarString() + "\n}"
    try {
      if ( debugger.value ) debugger.msg("Function is: "+body)
      val (f, returnType) = if ( ! functionMap2.contains(body) ) {
        val result = repl.interpret(body)
        if ( result != Success ) {
          if ( debugger.value ) debugger.msg("Error in defining function.")
          socket.putScalarInt(ERROR)
          return
        }
        val functionName = repl.mostRecentVar
        val f = repl.valueOfTerm(functionName).get
        val returnType = {
          val r = repl.symbolOfLine(functionName).info.toString.substring(10)  // Drop "String => " in the return type.
          if ( r.startsWith("iw$") ) r.substring(3)
          else r
        }
        functionMap2(body) = functionName
        functionMap(functionName) = (f, returnType)
        if ( debugger.value ) debugger.msg("Function definition is okay.")
        (f, returnType)
      } else functionMap(functionMap2(body))
      socket.putScalarInt(OK)
      socket.putScalarString(functionMap2(body))
    } catch {
      case e: Throwable =>
        socket.putScalarInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
    }
  }

  private def doInvoke(): Unit = {
    val functionName = socket.getScalarString()
    try {
      val (f, returnType) = functionMap(functionName)
      functionResult = (unary.invoke(f,socket.getScalarString()), returnType)
      R.exit()
      if ( debugger.value ) debugger.msg("Function invocation is okay.")
      socket.putScalarInt(OK)
    } catch {
      case e: Throwable =>
        R.exit()
        socket.putScalarInt(ERROR)
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
    socket.putScalarInt(OK)
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
          socket.putScalarInt(OK)
        } catch {
          case e: Throwable =>
            if ( debugger.value ) debugger.msg("Caught exception: "+e)
            socket.putScalarInt(ERROR)
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
          case INTEGER => (socket.getMatrixInt(nrow,ncol,rowMajor),    "Array[Array[Int]]")
          case DOUBLE =>  (socket.getMatrixDouble(nrow,ncol,rowMajor), "Array[Array[Double]]")
          case BOOLEAN => (socket.getMatrixBoolean(nrow,ncol,rowMajor),"Array[Array[Boolean]]")
          case STRING =>  (socket.getMatrixString(nrow,ncol,rowMajor), "Array[Array[String]]")
          case BYTE =>    (socket.getMatrixByte(nrow,ncol,rowMajor),   "Array[Array[Byte]]")
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
        socket.putScalarInt(ERROR)
        e.printStackTrace(pw)
        pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
        return
    }
    if ( debugger.value ) debugger.msg("Getting: "+identifier)
    if ( optionWithType._1.isEmpty ) {
      if ( optionWithType._2 == "<notype>" ) {
        if ( debugger.value ) debugger.msg("... which does not exist.")
        socket.putScalarInt(UNDEFINED_IDENTIFIER)
        return
      } else {
        if ( debugger.value ) debugger.msg("... which is empty [due to quirk in 2.10 series].")
        socket.putScalarInt(NULLTYPE)
        return
      }
    }
    val v = optionWithType._1.get
    if ( v == null || v.isInstanceOf[Unit] ) {
      if ( debugger.value ) debugger.msg("... which is null")
      socket.putScalarInt(NULLTYPE)
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
        socket.putTuple3Int(VECTOR,vv.length,INTEGER)
        socket.putVectorInt(vv)
      case "Array[Double]" =>
        val vv = v.asInstanceOf[Array[Double]]
        socket.putTuple3Int(VECTOR,vv.length,DOUBLE)
        socket.putVectorDouble(vv)
      case "Array[Boolean]" =>
        val vv = v.asInstanceOf[Array[Boolean]]
        socket.putTuple3Int(VECTOR,vv.length,BOOLEAN)
        socket.putVectorBoolean(vv)
      case "Array[String]" =>
        val vv = v.asInstanceOf[Array[String]]
        socket.putTuple3Int(VECTOR,vv.length,STRING)
        socket.putVectorString(vv)
      case "Array[Byte]" =>
        val vv = v.asInstanceOf[Array[Byte]]
        socket.putTuple3Int(VECTOR,vv.length,BYTE)
        socket.putVectorByte(vv)
      case "Array[Array[Int]]" =>
        val vv = v.asInstanceOf[Array[Array[Int]]]
        if ( socket.isMatrix(vv) ) {
          val (nrow,ncol) = socket.rowsColumns(vv,rowMajor)
          socket.putTuple4Int(MATRIX,nrow,ncol,INTEGER)
          socket.putMatrixInt(vv,rowMajor)
        } else socket.putScalarInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Double]]" =>
        val vv = v.asInstanceOf[Array[Array[Double]]]
        if ( socket.isMatrix(vv) ) {
          val (nrow,ncol) = socket.rowsColumns(vv,rowMajor)
          socket.putTuple4Int(MATRIX,nrow,ncol,DOUBLE)
          socket.putMatrixDouble(vv,rowMajor)
        } else socket.putScalarInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Boolean]]" =>
        val vv = v.asInstanceOf[Array[Array[Boolean]]]
        if ( socket.isMatrix(vv) ) {
          val (nrow,ncol) = socket.rowsColumns(vv,rowMajor)
          socket.putTuple4Int(MATRIX,nrow,ncol,BOOLEAN)
          socket.putMatrixBoolean(vv,rowMajor)
        } else socket.putScalarInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[String]]" =>
        val vv = v.asInstanceOf[Array[Array[String]]]
        if ( socket.isMatrix(vv) ) {
          val (nrow,ncol) = socket.rowsColumns(vv,rowMajor)
          socket.putTuple4Int(MATRIX,nrow,ncol,STRING)
          socket.putMatrixString(vv,rowMajor)
        } else socket.putScalarInt(UNSUPPORTED_STRUCTURE)
      case "Array[Array[Byte]]" =>
        val vv = v.asInstanceOf[Array[Array[Byte]]]
        if ( socket.isMatrix(vv) ) {
          val (nrow,ncol) = socket.rowsColumns(vv,rowMajor)
          socket.putTuple4Int(MATRIX,nrow,ncol,BYTE)
          socket.putMatrixByte(vv,rowMajor)
        } else socket.putScalarInt(UNSUPPORTED_STRUCTURE)
      case "Int" =>
        socket.putTuple2Int(SCALAR,INTEGER)
        socket.putScalarInt(v.asInstanceOf[Int])
      case "Double" =>
        socket.putTuple2Int(SCALAR,DOUBLE)
        socket.putScalarDouble(v.asInstanceOf[Double])
      case "Boolean" =>
        socket.putTuple2Int(SCALAR,BOOLEAN)
        socket.putScalarBoolean(v.asInstanceOf[Boolean])
      case "String" =>
        socket.putTuple2Int(SCALAR,STRING)
        socket.putScalarString(v.asInstanceOf[String])
      case "Byte" =>
        socket.putTuple2Int(SCALAR,BYTE)
        socket.putScalarByte(v.asInstanceOf[Byte])
      case _ =>
        if ( debugger.value ) debugger.msg("Bowing out: "+identifier)
        socket.putScalarInt(UNSUPPORTED_STRUCTURE)
    }
  }

  private def doGetReference(): Unit = {
    val identifier = socket.getScalarString()
    if ( debugger.value ) debugger.msg("Trying to get reference for: "+identifier)
    identifier match {
      case "?" =>
        socket.putScalarInt(OK)
        socket.putScalarString(cacheMap.store(functionResult))
        socket.putScalarString(functionResult._2)
      case cacheMap(value, typeOfTerm) =>
        socket.putScalarInt(OK)
        socket.putScalarString(identifier)
        socket.putScalarString(typeOfTerm)
      case _ =>
        if ( identifier == "null" ) {
          socket.putScalarInt(OK)
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
            socket.putScalarInt(ERROR)
            e.printStackTrace(pw)
            pw.println(e + ( if ( e.getCause != null ) System.lineSeparator + e.getCause else "" ) )
            return
        }
        if ( opt.isEmpty ) socket.putScalarInt(UNDEFINED_IDENTIFIER)
        else {
          socket.putScalarInt(OK)
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
      val request = try {
        socket.getScalarInt()
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

  def apply(snippetFilename: String, portsFilename: String, debug: Boolean = false, serializeOutput: Boolean = false, rowMajor: Boolean = true, port: Int = 0, prioritizeConnect: Boolean = true): ScalaServer = {
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
    // A better way to do it, but Scala 2.10.x does not the settings.language.add method.
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
    new ScalaServer(intp, prntWrtr, baosOut, baosErr, snippetFilename, portsFilename, debugger, serializeOutput, rowMajor, port, 1*1024*1024, prioritizeConnect)
  }

}

