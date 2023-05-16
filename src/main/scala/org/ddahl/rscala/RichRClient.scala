package org.ddahl.rscala


private[rscala] object RichClientUtil {
  /**
    * Get the R code for the specified return type.
    *
    * @param template the R template to evaluate
    * @param tag      the class tag of the return type
    * @return the R code for the specified return type
    */
  def getReturnType[T](template: String)(implicit tag: scala.reflect.ClassTag[T]): String = {
    tag.runtimeClass match {
      case c if c == classOf[RObject] => s"I(serialize({$template},NULL))"
      case c if c == classOf[Unit] || c == classOf[Nothing] => s"$template\nNULL"
      case _ =>
        val simpleName = tag.runtimeClass.getSimpleName
        val returnType = simpleName match {
          case int if int.startsWith("int") => "integer"
          case double if double.startsWith("double") => "double"
          case boolean if boolean.startsWith("boolean") => "logical"
          case byte if byte.startsWith("byte") => "raw"
          case string if string.startsWith("String") => "character"
          case _ => throw new IllegalArgumentException("Unsupported return type")
        }

        val storageMode = s"storage.mode(.rs) <- '$returnType';"

        val result = simpleName match {
          case matrix if matrix.endsWith("[][]") => s"$storageMode .rs"
          case vector if vector.endsWith("[]") => s"$storageMode I(.rs)"
          case "int" | "double" | "boolean" | "byte" | "String" => s"$storageMode .rs[1]"
        }
        s".rs <- {$template}; $result"
    }
  }

  import scala.reflect.runtime.universe._

  /**
    * This method takes a type parameter T with a context bound of TypeTag and returns a sequence of strings representing the names of the fields of the case class of type T.
    *
    * @tparam T the type of the case class
    * @return a sequence of strings representing the names of the fields of the case class of type T
    */
  def fieldsName[T: TypeTag]: Seq[String] = typeOf[T].members.collect {
    case m: MethodSymbol if m.isCaseAccessor => m.name.toString
  }.toSeq.reverse


}

object RichRClient {

  import scala.reflect.runtime.universe._

  implicit class RClientImplicit(r: RClient) {

    import RichClientUtil._

    // An implicit value of type scala.reflect.ClassTag[Unit] used as the default value for the tag parameter in the call function.
    implicit val defaultTag: scala.reflect.ClassTag[Unit] = scala.reflect.classTag[Unit]


    /**
      * Evaluate an R template with the specified values and return the result.
      *
      * @param template the R template to evaluate
      * @param values   the values to pass to the R template
      * @param tag      the class tag of the return type
      * @return the result of evaluating the R template
      */
    def call[T](template: String, values: Any*)(implicit tag: scala.reflect.ClassTag[T]): T = {
      val returnType: String = getReturnType[T](template)
      r.evalEngine(returnType, values)
      if (tag.runtimeClass == classOf[RObject]) {
        val x = r.server.conduit.pop[Array[Byte]]()
        new RObject(x).asInstanceOf[T]
      } else if (tag.runtimeClass == classOf[Unit]) {
        r.server.conduit.pop[Any]()
        ().asInstanceOf[T]
      } else {
        r.server.conduit.pop[T]()
      }

    }


    /**
      * This method takes an array of type T and returns an RObject representing a data frame.
      * It converts the input array into a string representation and passes it to an R script to create a data frame.
      *
      * @tparam T the type of the elements in the input array
      * @param seq the input array
      * @return an RObject representing a data frame
      */
    def createDataFrame[T <: Product : TypeTag](seq: Array[T]): RObject = {
      // Get the field names of type T using the fieldsName method
      val colNames = fieldsName[T]
      // Convert the input array into a string representation
      val values: String = seq.map(_.productIterator.map {
        // Handle special cases for NaN, null, and Byte values
        //        case b: Byte => "as.raw(" + b + ")"
        case true => "TRUE"
        case false => "FALSE"
        case byte: Byte =>
          r.server.debugger("byte to String")
          byte.toString
        case Double.NaN => "NA"
        case na if null == na => "NULL"
        // For other values, call toString and escape special characters
        case o =>
          o.toString
            .split("[()~]")
            .mkString("_")

      }.mkString("~"))
        .mkString("\n")

      // Construct the final string by concatenating the column names and values
      val str = colNames.mkString("", "~", "\n") + values

      // Construct an R script to create a data frame from the string representation
      val table = s"read.table(text='$str',header=TRUE,sep='~')"
      //      println(table)

      // Call the R script and return the result
      call[RObject](table)
    }


    /** This method takes an array of type T and an integer batchSize and returns an RObject representing a data frame.
      * It splits the input array into batches of size batchSize, calls createDataFrame on each batch, and combines the results using combineDataFrames.
      *
      * @tparam T the type of the elements in the input array
      * @param seq       the input array
      * @param batchSize the size of each batch
      * @return an RObject representing a data frame
      */
    def createDataFrame[T <: Product : TypeTag](seq: Array[T], batchSize: Int): RObject = {

      /**
        * This method takes a sequence of RObjects representing data frames and returns an RObject representing a combined data frame.
        * It constructs an R script to combine the input data frames using rbind and calls the script to return the result.
        *
        * @param dfs the sequence of RObjects representing data frames to combine
        * @return an RObject representing a combined data frame
        */
      def combineDataFrames(dfs: Seq[RObject]): RObject = {
        // Construct an R script to assign each input data frame to a variable
        val dfsString = dfs.zipWithIndex.map { case (df, i) => s"df$i <- %-" }.mkString("\n")
        // Construct an R script to combine the input data frames using rbind
        val rbindString = dfs.indices.map(i => s"df$i").mkString("rbind(", ",", ")")
        // Concatenate the scripts and add a line to return the result
        val script: String = dfsString + "\nresult <- " + rbindString + "\nresult\n"
        // Call the R script and return the result

        call[RObject](script, dfs: _*)
      }

      // Split the input array into batches and Call createDataFrame on each batch
      val dataFrames: Seq[RObject] = seq
        .grouped(batchSize)
        .map(createDataFrame[T])
        .toSeq

      // Combine the resulting data frames using combineDataFrames
      val result = combineDataFrames(dataFrames)
      result
    }

    //
    //    import scala.language.experimental.macros
    //    import scala.reflect.macros.blackbox.Context
    //
    //    def fieldsNameMacro[T]: Seq[String] = macro fieldsNameMacroImpl[T]
    //
    //    def fieldsNameMacroImpl[T: c.WeakTypeTag](c: Context): c.Expr[Seq[String]] = {
    //      import c.universe._
    //      val tpe = weakTypeOf[T]
    //      val fieldNames = tpe.decls.collect {
    //        case m: MethodSymbol if m.isCaseAccessor => m.name.toString
    //      }.toSeq.reverse
    //      c.Expr[Seq[String]](q"$fieldNames")
    //    }

  }

}
