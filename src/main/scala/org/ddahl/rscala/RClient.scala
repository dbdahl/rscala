package org.ddahl.rscala

import server._
import Protocol._
import scala.sys.process.Process

final class RObject private[rscala] (val x: Array[Byte]) {

  private def canEqual(a: Any): Boolean = a.isInstanceOf[RObject]

  override def equals(that: Any): Boolean = that match {
    case that: RObject => that.canEqual(this) && ( this.hashCode == that.hashCode ) && this.x.sameElements(that.x)
    case _ => false
  }

  override def hashCode: Int = java.util.Arrays.hashCode(x)

}

/** A bridge to R.
  *
  * In a Scala application, an instance of this class is created using its companion object as shown below.
  *
  * In an R script, the object `R` is an instance of this class available in an rscala bridge created by calling the function
  * `scala` from the package [[http://cran.r-project.org/package=rscala rscala]].  It is through this instance that
  * callbacks to the original [[http://www.r-project.org R]] interpreter are possible.
  *
  * All of the evaluation methods of this class have the same signature.  The first argument is a template for an R expression, where
  * `%-` is a placeholder for items that are provided as variable arguments.  The result type is indicated by the suffix of
  * the method name.  See examples below.
  *
  * This class is threadsafe.
  *
  * {{{
  * val R = org.ddahl.rscala.RClient()
  *
  * val a = R.evalD0("sd(rnorm(1000, mean=%-, sd=%-))", 1.0, 2.0)
  *
  * R.eval("primes <- %-", Array(2, 3, 5, 7, 11, 13, 17, 19, 23))
  * val rFunction = R.evalObject("function(x) x*primes")
  * val primesTimesTwo = R.evalI1("%-(2)", rFunction)
  *
  * val m = R.evalI2("matrix(rbinom(%-, size=10, prob=0.5), nrow=2)", 8)
  *
  * R.quit()
  * }}}
  *
  * @author David B. Dahl
  */
class RClient private[rscala] () {

  private[rscala] var server: Server = null
  private[rscala] var rProcessInstance: Process = null

  def quit(): Unit = synchronized {
    if ( rProcessInstance == null ) throw new IllegalStateException("The 'quit' method can only be called when R is embedded in Scala.")
    rProcessInstance.destroy()
  }

  def eval  (template: String, values: Any*): Unit                  = evalWithoutResult                     (template, values)

  def evalI0(template: String, values: Any*): Int                   = evalWithResult[Int]                   (template, values, "storage.mode(.rs) <- 'integer'; .rs[1]")
  def evalI1(template: String, values: Any*): Array[Int]            = evalWithResult[Array[Int]]            (template, values, "storage.mode(.rs) <- 'integer'; I(.rs)")
  def evalI2(template: String, values: Any*): Array[Array[Int]]     = evalWithResult[Array[Array[Int]]]     (template, values, "storage.mode(.rs) <- 'integer'; .rs")

  def evalD0(template: String, values: Any*): Double                = evalWithResult[Double]                (template, values, "storage.mode(.rs) <- 'double'; .rs[1]")
  def evalD1(template: String, values: Any*): Array[Double]         = evalWithResult[Array[Double]]         (template, values, "storage.mode(.rs) <- 'double'; I(.rs)")
  def evalD2(template: String, values: Any*): Array[Array[Double]]  = evalWithResult[Array[Array[Double]]]  (template, values, "storage.mode(.rs) <- 'double'; .rs")

  def evalL0(template: String, values: Any*): Boolean               = evalWithResult[Boolean]               (template, values, "storage.mode(.rs) <- 'logical'; .rs[1]")
  def evalL1(template: String, values: Any*): Array[Boolean]        = evalWithResult[Array[Boolean]]        (template, values, "storage.mode(.rs) <- 'logical'; I(.rs)")
  def evalL2(template: String, values: Any*): Array[Array[Boolean]] = evalWithResult[Array[Array[Boolean]]] (template, values, "storage.mode(.rs) <- 'logical'; .rs")

  def evalR0(template: String, values: Any*): Byte                  = evalWithResult[Byte]                  (template, values, "storage.mode(.rs) <- 'raw'; .rs[1]")
  def evalR1(template: String, values: Any*): Array[Byte]           = evalWithResult[Array[Byte]]           (template, values, "storage.mode(.rs) <- 'raw'; I(.rs)")
  def evalR2(template: String, values: Any*): Array[Array[Byte]]    = evalWithResult[Array[Array[Byte]]]    (template, values, "storage.mode(.rs) <- 'raw'; .rs")

  def evalS0(template: String, values: Any*): String                = evalWithResult[String]                (template, values, "storage.mode(.rs) <- 'character'; .rs[1]")
  def evalS1(template: String, values: Any*): Array[String]         = evalWithResult[Array[String]]         (template, values, "storage.mode(.rs) <- 'character'; I(.rs)")
  def evalS2(template: String, values: Any*): Array[Array[String]]  = evalWithResult[Array[Array[String]]]  (template, values, "storage.mode(.rs) <- 'character'; .rs")

  def evalObject(template: String, values: Any*): RObject = synchronized {
    val template2 = "I(serialize({" + template + "},NULL))"
    evalEngine(template2, values)
    val x = server.conduit.pop[Array[Byte]]
    new RObject(x)
  }

  private def evalWithoutResult[A](template: String, values: Seq[Any]): Unit = synchronized {
    evalEngine(template + "; NULL", values)
    server.conduit.pop[Any]()
  }

  private def evalWithResult[A](template: String, values: Seq[Any], casting: String): A = synchronized {
    evalEngine(".rs <- {" + template + "}; " + casting, values)
    server.conduit.pop[A]
  }

  private def evalEngine(template: String, values: Seq[Any]): Unit = synchronized {
    val values2 = values.map(v => any2Datum(v))
    server.pop(Datum(values.length, TCODE_CALLBACK, Some(template)))
    values2.foreach(server.pop)
    server.run()
    if ( server.getCmd() != PCODE_PUSH_WITHOUT_NAME ) throw new RuntimeException("Protocol error.")
    server.push(false)
  }

  private def any2Datum(any: Any): Datum = {
    val c = any.getClass
    val tipe = if (c.isArray) {
      c.getName match {
        case "[I" => TCODE_INT_1
        case "[D" => TCODE_DOUBLE_1
        case "[Z" => TCODE_LOGICAL_1
        case "[B" => TCODE_RAW_1
        case "[Ljava.lang.String;" => TCODE_CHARACTER_1
        case "[[I" => TCODE_INT_2
        case "[[D" => TCODE_DOUBLE_2
        case "[[Z" => TCODE_LOGICAL_2
        case "[[B" => TCODE_RAW_2
        case "[[Ljava.lang.String;" => TCODE_CHARACTER_2
        case _ => throw new RuntimeException("Unsupported array type.")
      }
    } else {
      c.getName match {
        case "java.lang.Integer" => TCODE_INT_0
        case "java.lang.Double" => TCODE_DOUBLE_0
        case "java.lang.Boolean" => TCODE_LOGICAL_0
        case "java.lang.Byte" => TCODE_RAW_0
        case "java.lang.String" => TCODE_CHARACTER_0
        case "org.ddahl.rscala.RObject" => TCODE_ROBJECT
        case o => throw new RuntimeException("Unsupported type: <"+o+">")
      }
    }
    if ( tipe == TCODE_ROBJECT ) Datum(any.asInstanceOf[RObject].x, tipe, None)
    else Datum(any, tipe, None)
  }

}


/** The companion object to the [[RClient]] class used to create an instance in a Scala application.
  *
  * {{{ val R = org.ddahl.rscala.RClient() }}}
  */
object RClient {

  import scala.sys.process._
  import java.io.{File, FileWriter, PrintWriter, BufferedReader, InputStreamReader, InputStream}
  import scala.language.dynamics
  import scala.sys.process.Process

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

  private def findROnWindows: String = {
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

  private lazy val allCodeInR = {
    val scripts = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/Rscripts")).getLines
    val codeInR = scripts.map(resource => {
      scala.io.Source.fromInputStream(getClass.getResourceAsStream(resource)).getLines.mkString("\n")
    }).mkString("\n\n")
    s"""
    rscala <- local({
      ${codeInR}
      environment()
    })"""
  }

  /** Returns an instance of the [[RClient]] class, using the path to the executable specified by `rCmd`.
    * If `port == 0`, the ports are randomly chosen.  Otherwise, the specified port and that port plus one are used.
    *
    * {{{val R = org.ddahl.rscala.RClient()}}}
    */
  def apply(rCmd: String = defaultRCmd, port: Int = 0, debug: Boolean = false): RClient = {
    val debugger = new Debugger(debug,new PrintWriter(System.out),"Scala",false)
    val sockets = new Sockets(port, true, debugger)
    var cmd: PrintWriter = null
    val command = rCmd +: ( defaultArguments ++ interactiveArguments )
    val processCmd = Process(command)
    var echo = false
    def reader(label: String)(input: InputStream) = {
      val in = new BufferedReader(new InputStreamReader(input))
      var line = in.readLine()
      while ( line != null ) {
        if ( echo ) println(label+line)
        line = in.readLine()
      }
      in.close()
    }
    val processIO = new ProcessIO(
      o => { cmd = new PrintWriter(o) },
      reader(""),
      reader(""),
      true
    )
    val rProcessInstance = processCmd.run(processIO)
    val sourceFile = File.createTempFile("rscala-","")
    val sourceFileNameForR = sourceFile.getAbsolutePath.replace(File.separator,"/")
    val writer = new FileWriter(sourceFile)
    writer.write(allCodeInR)
    writer.flush()
    writer.close()
    val snippet = s"""
      source("${sourceFileNameForR}")
      file.remove("${sourceFileNameForR}")
      rscala[['embeddedR']](c(${sockets.outPort},${sockets.inPort}),${if ( debugger.on ) "TRUE" else "FALSE"})
      q(save='no')
    """.stripMargin
    while ( cmd == null ) Thread.sleep(100)
    cmd.println(snippet)
    cmd.flush()
    val (out,in) = sockets.acceptAndSetup()
    val conduit = new Conduit(debugger)
    val prntWrtr = new PrintWriter(System.out)
    val server = new Server(null, sockets, null, conduit, out, in, debugger, false, prntWrtr, null)
    val rClient = new RClient()
    rClient.server = server
    rClient.rProcessInstance = rProcessInstance
    while ( sourceFile.exists() ) Thread.sleep(100)
    echo = true
    rClient
  }

}

