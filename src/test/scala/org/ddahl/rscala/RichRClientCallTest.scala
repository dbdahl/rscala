package org.ddahl.rscala


class RichRClientCallTest extends RTest {

  import RichRClient._

  test("IllegalArgumentException") {
    println(r.call("%-+3", 2) == ())
  }
  test("type") {
    assert(r.call[Unit]("%-+3", 2).isInstanceOf[Unit])
    assert(r.call[Int]("%-+3", 2) == 5)
    assert(r.call[Int]("%-*3", 2) == 6)
    assert(r.call[Double]("1.1") == 1.1)
    assert(r.call[Int]("1") == 1)
    assert(r.call[String]("'hello'") == "hello")
    assert(r.call[Boolean]("T"))
    println(r.call[Byte]("as.raw(1)"))
    assertThrows[IllegalArgumentException](r.call[Map[Int,Double]]("c(1.2,1.2)"))
    assert(!r.call[Boolean]("F"))
  }

  test("array type") {
    assert(r.call[Array[Boolean]]("c(T,T)").forall(identity))
    assert(!r.call[Array[Boolean]]("c(F,F)").exists(identity))
    assert(r.call[Array[String]]("c('a','b')") sameElements Array("a", "b"))
    assert(r.call[Array[Int]]("c(1,2,%-)", 3) sameElements Array(1, 2, 3))
    assert(r.call[Array[Int]]("c(1,2,%-)", 3) sameElements Array(1, 2, 3))
  }
  test("2*2 array type") {
    r.evalI2("matrix(1:4, nrow = 2)").foreach(x => println(x.mkString(",")))
    assert(r.evalI2("matrix(1:4, nrow = 2)").flatten sameElements Array(1, 3, 2, 4))
    assert(r.evalI2("array(1:4,dim=c(2,2))").flatten sameElements Array(1, 3, 2, 4))
    assert(r.call[Array[Array[Int]]]("array(1:4,dim=c(2,%-))", 2).flatten sameElements Array(1, 3, 2, 4))
  }
  test("ggplot") {
    val srcCode =
      """
        |library(ggplot2)
        |df <- data.frame(
        |  gp = factor(rep(letters[1:3], each = 10)),
        |  y = rnorm(30)
        |)
        |ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
        |  data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
        |}))
        |ggplot(df, aes(gp, y)) +
        |  geom_point() +
        |  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
        |""".stripMargin.replaceAll("\r\n|\r|\n", "\n")


    val obj1 = r.call[RObject](srcCode)

    r.call[Unit]("print(%-)", obj1)

  }

  test("rObject") {
    r.eval("primes <- %-", Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    val obj0 = r.evalObject("function(x) x * primes")
    val obj1 = r.call[RObject]("function(x) x * primes")
    val r1 = r.call[Array[Int]]("%-(%-)", obj1, 2).mkString(",")
    val r0 = r.evalI1("%-(%-)", obj0, 2).mkString(",")
    assert(r0 == r1)
  }


  test("performance") {

    val nums = 10

    val data = (0 to nums).flatMap(x =>
      (1 to 11).map(num => {
        val cmd = Array.fill(num)("%-").mkString("+")
        val input = (0 to num).toArray
        cmd -> input
      }))
    data.foreach(x => x._2.length)
    val loop = 10
    val newT =
      avgTime(loop) {
        for ((cmd, input) <- data) {
          r.call[Int](cmd, input: _*)
        }
      }
    val oldT = {
      avgTime(loop) {
        for ((cmd, input) <- data) {
          r.evalI1(cmd, input: _*)
        }
      }
    }
    val newT1 =
      avgTime(loop) {
        for ((cmd, input) <- data) {
          r.call[Int](cmd, input: _*)
        }
      }
    val oldT1 =
      avgTime(loop) {
        for ((cmd, input) <- data) {
          r.evalI1(cmd, input: _*)
        }
      }

    println(s"new :$newT old:$oldT ;")
    println(s"new :$newT1 old:$oldT1 ;")
  }
}
