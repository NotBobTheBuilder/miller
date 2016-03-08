package miller

import miller.ASTf.Program

import scala.io.StdIn

object Miller {

  def main (args: Array[String]): Unit = {
//    println("Common programs:\nvar add = function (x, y) { return x + y; }; add(1,2);")
//    val prog = Parsing.parse(StdIn.readLine("> "))

//    println(
//      Seq(
//        "var sub = function(a, b) { return a - b; }; sub('foo', 'bar');"
//      ).map(Inference.check).mkString("==\n", "==\n", "==")
//    // TODO : Generate buggy function call counterexamples
//    )

//    val p = Parsing.parse("var sub = function(a, b) { return a - b; };")
//    println(Parsing.parse("5 + 2 - 3 < 2 && 5 / 2 == 12 + 33  && 7 / 4 <= 5 "))

    val i = io.Source.stdin.getLines().mkString("\n")
    val p = Parsing.parse(i)
//    val p = Parsing.parse("function(a, b) { return a - b; }")
    println(p)
  }
}
