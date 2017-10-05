package org.ddahl.rscala.server

object Main extends App {

  ScalaServer(args(0), args(1) == "TRUE", args(2) == "TRUE", args(3) == "TRUE", args(4).toInt, args(5) == "TRUE").run()

}

