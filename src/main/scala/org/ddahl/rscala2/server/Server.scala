package org.ddahl.rscala2.server

import Protocol._
import java.io.{BufferedOutputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, File, PrintWriter}
import java.net.ServerSocket
import java.nio.ByteBuffer
import scala.tools.nsc.interpreter.{ILoop, IMain}
import scala.tools.nsc.interpreter.IR.Success
import scala.tools.nsc.Settings
import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object Server extends App {

  val rscalaClasspath = args(0)
  val port = args(1).toInt
  val portsFilename = args(2)
  val sessionFilename =  args(3)
  val debug = ( args(4) == "TRUE" )
  val serializeOutput = ( args(5) == "TRUE" )
  val buffer = ( args(6) == "TRUE" )

  object killer extends Thread {
    import collection.JavaConverters._
    import java.nio.file.{FileSystems, Path, Paths, StandardWatchEventKinds, NoSuchFileException}
    override def run(): Unit = {
      val watchService = FileSystems.getDefault().newWatchService()
      val sessionFileFullPath = Paths.get(sessionFilename)
      val sessionFile = sessionFileFullPath.getFileName
      try {
        sessionFileFullPath.getParent.register(watchService, StandardWatchEventKinds.ENTRY_DELETE)
      } catch {
        case e: NoSuchFileException => sys.exit(0)      // R already exited!
      }
      while (true) {
        val watchKey = watchService.take()
        for ( event <- watchKey.pollEvents().asScala ) {
          val path = event.context.asInstanceOf[Path]
          if ( path.equals(sessionFile) ) sys.exit(0)
        }
        watchKey.reset()
      }
    }
  }
  killer.setDaemon(true)
  killer.start()

  // Set classpath
  private val settings = new Settings()
  settings.embeddedDefaults[Datum]
  settings.classpath.value = rscalaClasspath
  settings.deprecation.value = true
  settings.feature.value = true
  settings.unchecked.value = true
  settings.language.add("reflectiveCalls")

  private val referenceMap = new HashMap[Int, (Any,String)]()
  private val embeddedStack = new EmbeddedStack(referenceMap)    // called ES in the REPL
  private val functionMap = new HashMap[String, (Any,String)]()
  private val unary = Class.forName("scala.Function0").getMethod("apply")
  unary.setAccessible(true)

  // Set up sinks
  val (debugger, prntWrtr, baosOut, baosErr) = serializeOutput match {
    case true =>
      val out = new ByteArrayOutputStream()
      val err = new ByteArrayOutputStream()
      val pw = new PrintWriter(out)
      val d = new Debugger(debug, pw, "Scala", false)
      (d, pw, out, err)
    case false =>
      val pw = new PrintWriter(System.out, true)
      val d = new Debugger(debug, pw, "Scala", false)
      (d, pw, null, null)
  }

  // Instantiate an interpreter
  val intp = new IMain(settings, prntWrtr)
  // Don't be chatty
  locally {
    val iloop = new ILoop()
    iloop.intp = intp
    iloop.verbosity()
  }
  intp.bind("ES",embeddedStack)

  private val zero = 0.toByte
  private val one = 1.toByte

  debugger("starting server")

  private val (out, in) = {
    val ( portS2R, portR2S) = if ( port == 0 ) (0, 0) else (port, port+1)
    val serverOut = new ServerSocket(portS2R)
    val serverIn = new ServerSocket(portR2S)
    locally {
      val portsFile = new File(portsFilename)
      val p = new PrintWriter(portsFile)
      p.println(serverOut.getLocalPort+" "+serverIn.getLocalPort)
      p.close()
    }
    if (debugger.on) debugger("socket S2R waiting for client on port: " + serverOut.getLocalPort)
    val sOut = serverOut.accept()
    if (debugger.on) debugger("socket R2S waiting for client on port: " + serverIn.getLocalPort)
    val sIn = serverIn.accept()
    val bos = if ( buffer ) new BufferedOutputStream(sOut.getOutputStream) else sOut.getOutputStream
    (new DataOutputStream(bos), new DataInputStream(sIn.getInputStream))
  }

  if ( debugger.on ) debugger("connections established")

  loop()

  def exit(): Unit = {
    if ( debugger.on ) debugger("exit")
    out.close()
    in.close()
  }

  private def readString(): String = {
    val nBytes = in.readInt()
    val bytes = new Array[Byte](nBytes)
    in.readFully(bytes)
    new String(bytes,"UTF-8")
  }

  private def writeString(x: String): Unit = {
    val bytes = x.getBytes("UTF-8")
    out.writeInt(bytes.length)
    out.write(bytes)
  }

  def push(): Unit = {
    if ( debugger.on ) debugger("push")
    val tipe = in.readByte()
    val value = tipe match {
      case TCODE_REFERENCE =>
        val (value,typeString) = referenceMap(in.readInt())
        embeddedStack.pushValue(Datum(value,tipe,Some(typeString)))
        return
      case TCODE_INT_0 =>
        in.readInt()
      case TCODE_INT_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_INT)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(len) { byteBuffer.getInt() }
      case TCODE_INT_2 =>
        val nRows = in.readInt()
        val nColumns = in.readInt()
        val bytes = new Array[Byte](nRows*nColumns*BYTES_PER_INT)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(nRows) { Array.fill(nColumns) { byteBuffer.getInt() } }
      case TCODE_DOUBLE_0 =>
        in.readDouble()
      case TCODE_DOUBLE_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len*BYTES_PER_DOUBLE)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(len) { byteBuffer.getDouble() }
      case TCODE_DOUBLE_2 =>
        val nRows = in.readInt()
        val nColumns = in.readInt()
        val bytes = new Array[Byte](nRows*nColumns*BYTES_PER_DOUBLE)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(nRows) { Array.fill(nColumns) { byteBuffer.getDouble() } }
      case TCODE_LOGICAL_0 =>
        if ( in.readByte() != zero ) true else false
      case TCODE_LOGICAL_1 =>
        val len = in.readInt()
        val bytes = new Array[Byte](len)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(len) { if ( byteBuffer.get() != zero ) true else false }
      case TCODE_LOGICAL_2 =>
        val nRows = in.readInt()
        val nColumns = in.readInt()
        val bytes = new Array[Byte](nRows*nColumns)
        in.readFully(bytes)
        val byteBuffer = ByteBuffer.wrap(bytes)
        Array.fill(nRows) { Array.fill(nColumns) { if ( byteBuffer.get() != zero ) true else false } }
      case TCODE_RAW_0 =>
        in.readByte()
      case TCODE_RAW_1 =>
        val len = in.readInt()
        val x = new Array[Byte](len)
        in.readFully(x)
        x
      case TCODE_RAW_2 =>
        val nRows = in.readInt()
        val nColumns = in.readInt()
        Array.fill(nRows) { val x = new Array[Byte](nColumns); in.readFully(x); x }
      case TCODE_CHARACTER_0 =>
        readString()
      case TCODE_CHARACTER_1 =>
        val len = in.readInt()
        Array.fill(len) { readString() }
      case TCODE_CHARACTER_2 =>
        val nRows = in.readInt()
        val nColumns = in.readInt()
        Array.fill(nRows) { Array.fill(nColumns) { readString() } }
      case _ =>
        throw new IllegalStateException("Unsupported type.")
    }
    embeddedStack.pushValue(Datum(value,tipe,None))
  }

  def report(datum: Datum): Unit = {
    if ( debugger.on ) debugger("report")
    embeddedStack.reset()
    val tipe = datum.tipe
    out.writeByte(tipe)
    tipe match {
      case TCODE_INT_0 =>
        out.writeInt(datum.value.asInstanceOf[Int])
      case TCODE_INT_1 =>
        val value = datum.value.asInstanceOf[Array[Int]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_INT)
        value.foreach(byteBuffer.putInt)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_INT_2 =>
        val value = datum.value.asInstanceOf[Array[Array[Int]]]
        val nRows = value.length
        val nColumns = value(0).length
        val byteBuffer = ByteBuffer.allocate(nRows*nColumns*BYTES_PER_INT)
        value.foreach(_.foreach(byteBuffer.putInt))
        out.writeInt(nRows)
        out.writeInt(nColumns)
        out.write(byteBuffer.array)
      case TCODE_DOUBLE_0 =>
        out.writeDouble(datum.value.asInstanceOf[Double])
      case TCODE_DOUBLE_1 =>
        val value = datum.value.asInstanceOf[Array[Double]]
        val byteBuffer = ByteBuffer.allocate(value.length*BYTES_PER_DOUBLE)
        value.foreach(byteBuffer.putDouble)
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_DOUBLE_2 =>
        val value = datum.value.asInstanceOf[Array[Array[Double]]]
        val nRows = value.length
        val nColumns = value(0).length
        val byteBuffer = ByteBuffer.allocate(nRows*nColumns*BYTES_PER_DOUBLE)
        value.foreach(_.foreach(byteBuffer.putDouble))
        out.writeInt(nRows)
        out.writeInt(nColumns)
        out.write(byteBuffer.array)
      case TCODE_LOGICAL_0 =>
        out.write(if ( datum.value.asInstanceOf[Boolean] ) one else zero)
      case TCODE_LOGICAL_1 =>
        val value = datum.value.asInstanceOf[Array[Boolean]]
        val byteBuffer = ByteBuffer.allocate(value.length)
        value.foreach(boolean => byteBuffer.put(if (boolean) one else zero))
        out.writeInt(value.length)
        out.write(byteBuffer.array)
      case TCODE_LOGICAL_2 =>
        val value = datum.value.asInstanceOf[Array[Array[Boolean]]]
        val nRows = value.length
        val nColumns = value(0).length
        val byteBuffer = ByteBuffer.allocate(nRows*nColumns)
        value.foreach(_.foreach(x => byteBuffer.put( if ( x ) one else zero)))
        out.writeInt(nRows)
        out.writeInt(nColumns)
        out.write(byteBuffer.array)
       case TCODE_RAW_0 =>
        out.writeByte(datum.value.asInstanceOf[Byte])
      case TCODE_RAW_1 =>
        val value = datum.value.asInstanceOf[Array[Byte]]
        out.writeInt(value.length)
        out.write(value)
      case TCODE_RAW_2 =>
        val value = datum.value.asInstanceOf[Array[Array[Byte]]]
        out.writeInt(value.length)
        out.writeInt(value(0).length)
        value.foreach(out.write)
      case TCODE_CHARACTER_0 =>
        writeString(datum.value.asInstanceOf[String])
      case TCODE_CHARACTER_1 =>
        val value = datum.value.asInstanceOf[Array[String]]
        out.writeInt(value.length)
        value.foreach(writeString)
      case TCODE_CHARACTER_2 =>
        val value = datum.value.asInstanceOf[Array[Array[String]]]
        out.writeInt(value.length)
        out.writeInt(value(0).length)
        value.foreach(_.foreach(writeString))
      case TCODE_UNIT =>
      case TCODE_REFERENCE =>
        val key = {
          var candidate = scala.util.Random.nextInt()
          while ( referenceMap.contains(candidate) ) {
            candidate = scala.util.Random.nextInt()
          }
          candidate
        }
        val tipeString = datum.tipeString.get
        referenceMap(key) = (datum.value, tipeString)
        out.writeInt(key)
        writeString(tipeString)
      case TCODE_ERROR_DEF =>
        writeString(datum.value.asInstanceOf[String])
      case TCODE_ERROR_INVOKE =>
      case e =>
        throw new IllegalStateException("Unsupported type: "+e)
    }
    out.flush()
  }

  def invoke(withNames: Boolean, withReference: Boolean, showCode: Boolean): Unit = {
    if ( debugger.on ) debugger("invoke with" + (if (withNames) "" else "out") +" names")
    if ( withNames ) List.fill(embeddedStack.size)(readString()).foreach(embeddedStack.pushName)
    val snippet = readString()
    val sb = new java.lang.StringBuilder()
    sb.append("() => {\n")
    sb.append(embeddedStack)
    if ( withReference ) {
      sb.append("x")
      sb.append(embeddedStack.size)
      sb.append(".")
    }
    val forceReference = if ( snippet.startsWith(".") ) {
      if ( snippet.startsWith(".new_") ) {
        sb.append("new ")
        sb.append(snippet.substring(5))
      } else if ( snippet.startsWith(".null_") ) {
        sb.append("null: ")
        sb.append(snippet.substring(6))
      } else {
        sb.append(snippet.substring(1))
      }
      true
    } else {
      sb.append(snippet)
      false
    }
    if ( ! withNames ) sb.append(embeddedStack.argsList(withReference))
    sb.append("\n}")
    val body = sb.toString
    if ( showCode ) println(body)
    val (jvmFunction, resultType) = functionMap.getOrElse(body, {
      val result = intp.interpret(body)
      if ( result != Success ) {
        if ( debugger.on ) debugger("error in defining function.")
        report(Datum(body,TCODE_ERROR_DEF,None))
        return
      }
      val functionName = intp.mostRecentVar
      val jvmFunction = intp.valueOfTerm(functionName).get
      val resultType = {
        val r = intp.symbolOfLine(functionName).info.toString.substring(6)  // Drop "() => " in the return type.
        if ( r.startsWith("iw$") ) r.substring(3)
        else r
      }
      if ( debugger.on ) debugger("result type: "+resultType)
      val tuple = (jvmFunction, resultType)
      functionMap(body) = tuple
      if ( debugger.on ) debugger("function definition is okay.")
      tuple
    })
    val result = try {
      unary.invoke(jvmFunction)
    } catch {
      case e: Throwable =>
        println(e)
        e.printStackTrace()
        if ( debugger.on ) debugger("error in executing function.")
        report(Datum(e,TCODE_ERROR_INVOKE,None))
        return
    }
    if ( debugger.on ) debugger("function invocation is okay.")
    val tipe = if ( ( ! forceReference ) && typeMapper2.contains(resultType) ) {
      val tipe = typeMapper2.get(resultType).get
      tipe match {
        case TCODE_INT_2 | TCODE_DOUBLE_2 | TCODE_LOGICAL_2 | TCODE_CHARACTER_2 =>
          val a = result.asInstanceOf[Array[Array[_]]]
          if ( a.length == 0 ) TCODE_REFERENCE
          else {
            val nColumns = a(0).length
            if ( a.forall(_.length == nColumns) ) tipe else TCODE_REFERENCE
          }
        case _ =>
          tipe
      }
    } else TCODE_REFERENCE
    if ( tipe == TCODE_REFERENCE ) {
      report(Datum(result, tipe, Some(resultType)))
    } else {
      report(Datum(result, tipe, None))
    }
  }

  def evaluate(): Unit = {
    if ( debugger.on ) debugger("evaluate")
    val body = readString()
    try {
      val result = intp.interpret(body)
      if ( result != Success ) {
        if ( debugger.on ) debugger("error in defining evaluation.")
        report(Datum(body,TCODE_ERROR_DEF,None))
        return
      }
    } catch {
      case e: Throwable =>
        println(e)
        e.printStackTrace()
        if ( debugger.on ) debugger("error in executing evaluation.")
        report(Datum(e,TCODE_ERROR_INVOKE,None))
        return
    }
    report(Datum((),TCODE_UNIT,None))
  }

  def gc(): Unit = {
    if ( debugger.on ) debugger("garbage collect")
    (0 until in.readInt()).foreach { i =>
      val key = in.readInt()
      referenceMap.remove(key)
    }
  }

  @tailrec
  def loop(): Unit = {
    if ( debugger.on ) debugger("main")
    val request = in.readByte()
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH => push()
      case PCODE_INVOKE_WITH_NAMES => invoke(true, false, debugger.on)
      case PCODE_INVOKE_WITHOUT_NAMES => invoke(false, false, debugger.on)
      case PCODE_INVOKE_WITH_REFERENCE => invoke(false, true, debugger.on)
      case PCODE_EVALUATE => evaluate()
      case PCODE_GARBAGE_COLLECT => gc()
      case _ =>
        throw new IllegalStateException("Unsupported command: "+request)
    }
    loop()
  }

}

