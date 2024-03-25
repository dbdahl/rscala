package org.ddahl.rscala



import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach

import java.io.File


abstract class RTest extends AnyFunSuite with BeforeAndAfterEach {

  // need setting environment variable RPATH
  private [this] val path: String = System.getenv("RPATH") + File.separator + "bin" + File.separator + "R.exe"

  var rWrapper: Option[RClient] = None

  lazy val  r: RClient = rWrapper.get

//  implicit lazy val RClientObj: RClient =r

  override def beforeEach(): Unit = {
    println("r server startup")
    rWrapper = Some(RClient(path, debug = false))
  }

  override def afterEach(): Unit = {
    println("r server shutdown")
    rWrapper.get.quit()
  }

  def avgTime(nums: Int)(f: => Unit): Long = {
    val result = (0 until nums).map(x => timeSpend(f))
    result.sum / result.size / 1000 / 1000
  }

  def timeSpend(f: => Unit): Long = {
    val start = System.nanoTime()
    f
    System.nanoTime() - start
  }

  def time(name: String)(f: => Unit) {

    println(name + ":" + timeSpend(f) / 1000 / 1000)
  }

}
