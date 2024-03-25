package org.ddahl.rscala


import java.sql.Timestamp
import java.time.LocalDate


class RichClientCreateDataFrameTest extends RTest {

  import RichRClient._
  import r.implicits._

  val ts = (i: Int) => Timestamp.valueOf(LocalDate.now().plusDays(i) + " 00:00:00")
  val seq: Array[TestData] = (0 until 10).map(i => TestData(ts(i), i, "a" + i, i + 1.1 + i % 2, i.toByte, (i % 2, " ", i + "_h"), i % 2 == 0)).toArray
  val simpleSeq: Array[(Int, String, Double, Boolean, Byte)] = (1 to 10).map(x => (x, "a", x + 1.2, x % 2 == 0, "1".toByte)).toArray
  test("simple") {
    val df = r.createDataFrame[(Int, String, Double, Boolean, Byte)](simpleSeq)
//    print(df)
    view(df)
    printSchema(df)

  }
  test("product rec") {
    val seq: Array[(Int, (Int, Int))] = (1 to 10).map(x => (x, (1, 2))).toArray
    val df = r.createDataFrame[(Int, (Int, Int))](seq)
    assert(schema(df).toSeq == Seq("X_1" -> "integer", "X_2" -> "character"))
  }

  test("createDataFrame") {

    val srcCode =
      """
        |library(ggplot2)
        |ggplot(%-, aes(f, c,group=g,color=g)) +
        |  geom_point() +
        |  geom_line()
        |""".stripMargin.replaceAll("\r", "")


    val df = r.createDataFrame[TestData](seq :+ seq.head.copy(b = null, c = Double.NaN))
    r.call[Unit]("print(colnames(%-))", df)
    r.call[Unit]("print(df[,1])", df)
    val p = r.call[RObject](srcCode, df)


    view(df)
    print(df)
    print(p)
    printSchema(df)

  }

  test("createDataFrameBig") {
    val seqBig: Array[TestData] = (0 until 1000).flatMap(i => seq).toArray


    time("single") {
      val df = r.createDataFrame[TestData](seqBig)
      printDim(df)
    }

    time("batch") {
      val df = r.createDataFrame[TestData](seqBig, 10000)
      printDim(df)
    }

    time("batch") {
      val df = r.createDataFrame[TestData](seqBig, 5000)
      printDim(df)
    }


  }

  test("xs") {


    val df = r.createDataFrame[TestData](seq, 2)

    r.call[Unit]("print(dim(%-))", df)

  }


}

case class TestData(f: Timestamp,
                    a: Int,
                    b: String,
                    c: Double,
                    d: Byte,
                    g: (Int, String, String),
                    e: Boolean)
