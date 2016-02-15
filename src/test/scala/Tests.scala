import miller.ASTf.{Add, Expr, Program}
import miller._

object Tests {
  def main(args: Array[String]) {
    Seq[String](
      "1+2+3+4;",

      "var a = true;",

      "2*2*3/4+10;",

      "100;",

      "'hello ' + 'world';",

      "function () {};",

      "function (a,b,c) {};",

      "function (a,b,c) {return 3;};",

      "function foo (a,b,c) {return 3;};",

      "var a = 1; var b = 2; var c = 3;",

      "var a; var b; a + b;",

      "var a; var b; a + b + 1;",

      "var alert = function (i) {}; var a = 1; var b = 2; var c = 3; alert(a + b + c); ",

      "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",

      "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",

      "var add = function (a, b) {return a + b}; var b = add(1, 2);",

      "function (a, b) {return a - b};",

      "var add = function (a, b) { return function(c) { return a + b + c } }"

    ) foreach { code =>
      try {
        println(Parsing.parse(code))
        println()
      } catch {
        case a: Exception => a.printStackTrace(); println("Error: " + code)
      }
    }
  }
}

class ParsedData(
  val program: Program
)