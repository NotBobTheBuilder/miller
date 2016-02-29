import miller.ASTf.{Add, Expr, Program}
import miller._

object Checker {
  def main() {
    val (passes, fails) = Seq[(Boolean, String)](
      true -> "1+2+3+4;",
      true -> "var a = true; a = false",
      true -> "2*2*3/4+10;",
      true -> "100;",
      true -> "'hello ' + 'world';",
      true -> "function () {};",
      true -> "function (a,b,c) {};",
      true -> "function (a,b,c) {return 3;};",
      true -> "function foo (a,b,c) {return 3;};",
      true -> "var a = 1; var b = 2; var c = 3;",
      true -> "var alert = function (i) {}; var a = 1; var b = 2; var c = 3; alert(a + b + c); ",
      true -> "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",
      true -> "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",
      true -> "var add = function (a, b) {return a + b}; var b = add(1, 2);",
      true -> "function (a, b) {return a - b};",
      true -> "var add = function (a, b) { return function(c) { return a + b + c } }",

      false -> "1+2+3+'Hello';",
      false -> "'hello' - 2;",
      false -> "var a = 'hello'; a - 2;",

      false -> "var a = 'hello'; function foo() { if (a + 3) { var c = 2 * a; } };",
      true -> "var a = 'hello'; function foo() { var a = 1; if (a + 3) { var c = 2 * a; } };",

      false -> "var a = 'hello'; a = 4;",
      false -> "var add = function(a, b) { return a + b; }; add(1);",
      true -> "var add = function(a, b) { return a + b; }; add(1, 2);",
      true -> "var add = function(a, b) { return a + b; }; add('foo', 'bar');",
      false -> "var sub = function(a, b) { return a - b; }; sub('foo', 'bar');",
      false -> "var add = function(a, b) { return a + b; }; add('foo', 1);"

    ).foldLeft((0,0)) { (c, code) =>
      val (ps, fs) = c
      try {

        val (res, errs) = Inference.test(code._2)
        if (res == code._1) {
          (ps + 1, fs)
        } else {
          println(s"Expected ${code._1}, got $res on ${code._2}")
          (ps, fs + 1)
        }
      } catch {
        case e: Exception =>
          println(s"Error on ${code._2} $e")
          (ps, fs + 1)
      }
    }

    println(s"passes: $passes\nfails: $fails")
    if (fails > 0) {
      System.exit(1)
    }
  }
}

class ParsedData(
  val program: Program
)