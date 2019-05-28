import org.ddahl.rscala._

object HelloWorldRscala {

  def main(argv: Array[String]): Unit = {
    println("Hello World!")
    val R = RClient()
    println("Goodbye!")
  }

}

