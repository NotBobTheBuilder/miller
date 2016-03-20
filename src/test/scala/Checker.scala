import miller.ASTf.{Add, Expr, Program}
import miller._

object Checker {
  def main() {
    val (passes, fails) = Seq(
    true  -> "1+2+3+4;",
    true  -> "var a = true; a = false",
    true  -> "2*2*3/4+10;",
    true  -> "100;",
    true  -> "'hello ' + 'world';",
    true  -> "function () {};",
    true  -> "function (a,b,c) {};",
    true  -> "function (a,b,c) {return 3;};",
    true  -> "function foo (a,b,c) {return 3;};",
    true  -> "var a = 1; var b = 2; var c = 3;",
    true  -> "var alert = function (i) {}; var a = 1; var b = 2; var c = 3; alert(a + b + c); ",
    true  -> "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",
    true  -> "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;",
    true  -> "var add = function (a, b) {return a + b}; var b = add(1, 2);",
    true  -> "function (a, b) {return a - b};",
    true  -> "var add = function (a, b) { return function(c) { return a + b + c } }",

    false -> "1+2+3+'Hello';",
    false -> "'hello' - 2;",
    false -> "var a = 'hello'; a - 2;",

    false -> "var a = 'hello'; function foo() { if (a + 3) { var c = 2 * a; } };",
    true  -> "var a = 'hello'; function foo(a) { if (a + 3) { var c = 2 * a; } };",
    true  -> "var a = 'hello'; function foo() { var a = 1; if (a + 3) { var c = 2 * a; } };",

    false -> "var a = 'hello'; a = 4;",
    false -> "var add = function(a, b) { return a + b; }; add(1);",
    true  -> "var add = function(a, b) { return a + b; }; add(1, 2);",
    true  -> "var add = function(a, b) { return a + b; }; add('foo', 'bar');",
    false -> "var sub = function(a, b) { return a - b; }; sub('foo', 'bar');",
    false -> "var add = function(a, b) { return a + b; }; add('foo', 1);",
    true  -> "(function(a) { return function(b) { return a + b; }; })(1)(2)",
    false -> "(function(a) { return function(b) { return a + b; }; })(1)('2')",

    true  -> "var add = function(a) { return function(b) { return a + b; }; }; add(1)(2)",
    true  -> "var add = function(a) { return function(b) { return a + b; }; }; add('1')('2')",
    false -> "var add = function(a) { return function(b) { return a + b; }; }; add(1)('2')",

    true  -> "var add = function(a) { return function(b) { return a + b; }; }; add(1)(2); add('1')('2');",
    false -> "var add = function(a) { return function(b) { return a + b; }; }; add(1)(2); add('1')('2'); add(1)('2')",

    true  -> "var sub = function(a) { return function(b) { return a - b; }; }; sub(1)(2)",

    true  -> "function() { if (true) { return 1; } else { return 2; }; }",
    false -> "function() { if (true) { return 1; } else { return '2'; }; }",

    true  -> "var x = {a: 1, b: 2}; x.a + x.b; x.a - x.b;",
    true  -> "var x = {a: '1', b: '2'}; x.a + x.b;",
    false -> "var x = {a: 1, b: '2'}; x.a + x.b; x.a - x.b;",
    true  -> "var x = {add: function(a, b) { return a + b; }, x: 1, y: 2, a: 3, b: 4}; x.add(x.add(x.x, x.y), x.add(x.a, x.b));",

    true  -> "var x = {}; x.add = function(a, b) { return a + b }; x.add(1, 2);",
    false -> "var x = {}; x.add = function(a, b) { return a + b }; x.add(1, '2');",

    true  -> "var x = [1, 2, 3, 4, 5]; 2 + x[0]",
    false -> "var x = ['1', '2', '3', '4', '5']; 2 + x[0]",

    true  -> "var x = [{a: 1}, {a: 2}, {a: 3}]",
    false -> "var x = [{a: 1}, {b: 2}, {c: 3}]",
    true  -> "var x = [{name: 'Jack', age: 22}, {make: 'Citroen C1', age: 6}]"

    ).foldLeft((0,0)) { case ((ps, fs), (expected, code)) =>
      try {
        val (res, errs) = Inference.test(code)
        if (res == expected) {
          (ps + 1, fs)
        } else {
          println(s"Expected $expected, got $res on $code")
          (ps, fs + 1)
        }
      } catch {
        case e: Exception =>
          println(s"Error on $code $e")
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