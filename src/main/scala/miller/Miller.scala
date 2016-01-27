package miller

import scala.io.StdIn

object Miller {

  def main (args: Array[String]): Unit = {
    println(Parsing.parse(StdIn.readLine("> ")))
  }

}
