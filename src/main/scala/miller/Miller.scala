package miller

import miller.ASTf.Program

import scala.io.StdIn

object Miller {

  def main (args: Array[String]): Unit = {
    /*
      var add = function (x, y) { return x + y; }; add(1,2);

     */
//    println("Common programs:")
//    println("var add = function (x, y) { return x + y; }; add(1,2);")

//    val prog = Parsing.parse(StdIn.readLine("> "))
    println(
      Seq(
        "1+2+3+'Hello';",
        "1+2+3;",
        "'hello' - 2;",
        "var a = 'hello'; a - 2;",
        "var a = 'hello'; function foo() { if (a + 3) { var c = 2 * a; } };",
        "var a = 'hello'; function foo() { var a = 1; if (a + 3) { var c = 2 * a; } };",
        "var a = 'hello'; a = 4;",
        "var add = function(a, b) { return a + b; }; add(1);",
        "var add = function(a, b) { return a + b; }; add(1, 2);",
        "var add = function(a, b) { return a + b; }; add('foo', 'bar');",
        "var add = function(a, b) { return a + b; }; add('foo', 1);",
        "var sub = function(a, b) { return a - b; }; sub('foo', 'bar');"
      ).map(Inference.check).mkString("==\n", "==\n", "==")
    )

    println(Parsing.parse("var add = function (a, b) { return a + b }; add(1);"))

  }

}
