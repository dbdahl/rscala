package org.ddahl.rscala.library

import org.ddahl.rscala.RObject

class LibraryBase extends RLibrary {
  import org.ddahl.rscala.RichRClient._

  def colNames(df: RObject): Array[String] = {
    r.call[Array[String]]("colnames(%-)", df)
  }

  def types(df: RObject): Array[String] = {
    val nums = r.call[Int]("dim(%-)[2]", df)
    r.call[Array[String]]("sapply(1:%-,function(x){typeof(%-[,x])})", nums, df)
  }

  def printDim(df:RObject):Unit={
    r.call[Unit]("print(dim(%-))", df)
  }

  def library(name: String): Unit = {
    r.call[Unit](s"library(${name})")
  }

  def library(): Unit = {
    r.call[Unit]("library()")
  }

  def ls(): Array[String] = {
    r.call[Array[String]]("ls()")
  }

  def printSchema(df: RObject): Unit = {
   schema(df).foreach(println)
  }
  def schema(df: RObject): Array[(String,String)] = {
    colNames(df).zip(types(df))
  }

  def view(df: RObject): Unit = {
    r.call[Unit]("View(%-)", df)
  }

  def print(obj: RObject): Unit = {
    r.call[Unit]("print(%-)", obj)
  }

}