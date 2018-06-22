package org.ddahl.rscala

import Protocol._
import java.io.{ByteArrayOutputStream, DataInputStream, DataOutputStream, File, PrintWriter}
import java.nio.ByteBuffer
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.IR.Success
import scala.annotation.tailrec
import scala.collection.mutable.HashMap

class Server(intp: IMain, out: DataOutputStream, in: DataInputStream, val debugger: Debugger, val serializeOutput: Boolean, prntWrtr: PrintWriter, baos: ByteArrayOutputStream) {

  private val zero = 0.toByte
  private val one = 1.toByte

  private val referenceMap = new HashMap[Int, (Any,String)]()
  private val functionMap = new HashMap[String, (Any,String)]()
  private val unary = Class.forName("scala.Function0").getMethod("apply")
  unary.setAccessible(true)

  val conduit = new Conduit(referenceMap, debugger)
  intp.bind("conduit",conduit)
  private val rClient = new RClient(this)
  intp.bind("R",rClient)

  private def exit(): Unit = {
    if ( debugger.on ) debugger("exit.")
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

  private def wrap[A](expression: => A): A = {
    if ( serializeOutput ) {
      scala.Console.withOut(baos) {
        scala.Console.withErr(baos) {
          expression
        }
      }
    } else expression
  }

  private[rscala] def push(withName: Boolean): Unit = {
    if ( debugger.on ) debugger("push with" + (if (withName) "" else "out") +" name.")
    val tipe = in.readByte()
    val value = tipe match {
      case TCODE_REFERENCE =>
        val (value,typeString) = referenceMap(in.readInt())
        val nameOption = if ( withName ) Some(readString()) else None
        if ( debugger.on && withName ) debugger("name is " + nameOption.get + ".")
        conduit.push(Datum(value,tipe,Some(typeString)),nameOption)
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
      case TCODE_UNIT =>
      case _ =>
        throw new IllegalStateException("Unsupported type.")
    }
    val nameOption = if ( withName ) Some(readString()) else None
    if ( debugger.on && withName ) debugger("name is " + nameOption.get + ".")
    conduit.push(Datum(value,tipe,None),nameOption)
  }

  private def clear(): Unit = {
    if ( debugger.on ) debugger("clear.")
    conduit.reset(in.readInt())
  }

  private[rscala] def report(datum: Datum): Unit = {
    if ( debugger.on ) debugger("report.")
    if ( serializeOutput ) {
      prntWrtr.flush()
      writeString(baos.toString)
      baos.reset()
    }
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
        val tipeString = datum.msg.get
        referenceMap(key) = (datum.value, tipeString)
        out.writeInt(key)
        writeString(tipeString)
      case TCODE_ERROR_DEF =>
        writeString(datum.value.asInstanceOf[String])
      case TCODE_ERROR_INVOKE =>
      case TCODE_CALLBACK =>
        writeString(datum.msg.get)
        out.writeInt(datum.value.asInstanceOf[Int])
      case e =>
        throw new IllegalStateException("Unsupported type: "+e)
    }
    out.flush()
  }

  private def invoke(withReference: Boolean, freeForm: Boolean): Unit = {
    if ( debugger.on ) debugger("invoke with" + (if (withReference) "" else "out") +" reference.")
    val nArgs = in.readInt()
    val snippet = readString()
    val header = conduit.mkHeader(nArgs)
    val sb = new java.lang.StringBuilder()
    sb.append("() => {\n")
    sb.append(header)
    if ( withReference ) {
      sb.append("x")
      sb.append(nArgs)
      sb.append(".")
    }
    val forceReference = if ( snippet.startsWith(".") ) {
      if ( snippet.startsWith(".new_") ) {
        sb.append("new ")
        sb.append(snippet.substring(5))
      } else if ( snippet.startsWith(".null_") ) {
        sb.append("null: ")
        sb.append(snippet.substring(6))
      } else if ( snippet.startsWith(".asInstanceOf_") ) {
        sb.append("asInstanceOf[")
        sb.append(snippet.substring(14))
        sb.append("]")
      } else {
        sb.append(snippet.substring(1))
      }
      true
    } else {
      sb.append(snippet)
      false
    }
    if ( ! freeForm ) sb.append(conduit.argsList(nArgs, withReference))
    sb.append("\n}")
    val body = sb.toString
    if ( conduit.showCode || debugger.on ) {
      prntWrtr.println("Generated code:")
      prntWrtr.println(body)
    }
    val (jvmFunction, resultType) = functionMap.getOrElse(body, {
      val result = wrap(intp.interpret(body))
      if ( result != Success ) {
        if ( debugger.on ) debugger("error in defining function.")
        conduit.reset(nArgs)
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
      if ( debugger.on ) debugger("function definition is okay, result type " + resultType + ".")
      val tuple = (jvmFunction, resultType)
      functionMap(body) = tuple
      tuple
    })
    val result = try {
      wrap(unary.invoke(jvmFunction))
    } catch {
      case e: Throwable =>
        prntWrtr.println(e)
        if ( e.getCause != null ) e.getCause.printStackTrace(prntWrtr) else e.printStackTrace(prntWrtr)
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

  private def evaluate(): Unit = {
    if ( debugger.on ) debugger("evaluate.")
    val body = readString()
    try {
      val result = wrap(intp.interpret(body))
      if ( result != Success ) {
        if ( debugger.on ) debugger("error in defining evaluation.")
        report(Datum(body,TCODE_ERROR_DEF,None))
        return
      }
    } catch {
      case e: Throwable =>
        prntWrtr.println(e)
        if ( e.getCause != null ) e.getCause.printStackTrace(prntWrtr) else e.printStackTrace(prntWrtr)
        if ( debugger.on ) debugger("error in executing evaluation.")
        report(Datum(e,TCODE_ERROR_INVOKE,None))
        return
    }
    report(Datum((),TCODE_UNIT,None))
  }

  private def addToClasspath(): Unit = {
    if ( debugger.on ) debugger("add to classpath.")
    val body = readString()
    try {
      val path = new File(body).toURI.toURL
      intp.addUrlsToClassPath(path)
    } catch {
      case e: Throwable =>
        prntWrtr.println(e)
        e.printStackTrace(prntWrtr)
        if ( debugger.on ) debugger("error in adding to classpath.")
        report(Datum(e,TCODE_ERROR_INVOKE,None))
        return
    }
    report(Datum((),TCODE_UNIT,None))
  }

  private def gc(): Unit = {
    if ( debugger.on ) debugger("garbage collect.")
    (0 until in.readInt()).foreach { i =>
      val key = in.readInt()
      referenceMap.remove(key)
    }
  }

  @tailrec
  final def run(oneOff: Boolean): Unit = {
    if ( debugger.on ) debugger("main, stack size = " + conduit.size + ".")
    val request = try {
      in.readByte()
    } catch {
      case e: Throwable =>
        if ( debugger.on ) {
          prntWrtr.println(e)
          e.printStackTrace(prntWrtr)
          debugger("fatal error at loop main.")
        }
        return
    }
    request match {
      case PCODE_EXIT => exit(); return
      case PCODE_PUSH_WITH_NAME => push(true)
      case PCODE_PUSH_WITHOUT_NAME => push(false)
      case PCODE_CLEAR => clear()
      case PCODE_INVOKE => invoke(false, false)
      case PCODE_INVOKE_WITH_REFERENCE => invoke(true,false)
      case PCODE_INVOKE_FREEFORM => invoke(false,true)
      case PCODE_EVALUATE => evaluate()
      case PCODE_ADD_TO_CLASSPATH => addToClasspath()
      case PCODE_GARBAGE_COLLECT => gc()
      case _ =>
        throw new IllegalStateException("Unsupported command: "+request)
    }
    if ( ! oneOff ) run(false)
  }

}

