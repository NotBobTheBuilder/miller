package miller

import scala.io.Source.stdin

object Miller {
  def main(args: Array[String]): Unit = {
    var code = stdin.getLines().mkString("\n")

    println(Parsing.parse(code))
    println(Inference.check(code))
    // TODO : Generate buggy function call counterexamples
  }
}
