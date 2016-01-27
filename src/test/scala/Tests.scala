import miller.ASTf.{Add, Expr, Program}
import miller._

object Tests {
  def main(args: Array[String]) {
    Seq[(String, (ParsedData) => Boolean)](
      "1+2+3+4;"
        -> (_.program.statements.headOption.collect({ case x: Expr => x.t.isA(TNumber) }).isDefined),

      "var a = true;"
        -> (_.program.stack.typeOf("a").collect({ case ConstT(TBoolean) => true}).nonEmpty),

      "2*2*3/4+10;"
        -> (_.program.statements.headOption.collect({ case x: Expr => x.t.isA(TNumber) }).isDefined),

      "100;"
        -> (_.program.statements.headOption.collect({ case x: Expr => x.t.isA(TNumber) }).isDefined),

      "'hello ' + 'world';"
        -> (_.program.statements.headOption.collect({ case Add(_, _, ConstT(TString)) => true }).isDefined),

      "function () {};"
        -> (_.program.statements.head.asInstanceOf[Expr].t.isA(TFunction(Seq(), ConstT(TUndefined)))),

      "function (a,b,c) {};"
        -> (_.program.statements.head.asInstanceOf[Expr].t.isA(TFunction(Seq(), ConstT(TUndefined)))),

      "function (a,b,c) {return 3;};"
        -> (p => false),

      "function fleh (a,b,c) {return 3;};"
        -> (p => false),

      "var a = 1; var b = 2; var c = 3;"
        -> (p => false),

      "var a; var b; a + b;"
        -> (p => p.program.stack.typeOf("a").nonEmpty && p.program.stack.typeOf("b").nonEmpty),

      "var a; var b; a + b + 1;"
        -> (p => false),

      "var alert = function (i) {}; var a = 1; var b = 2; var c = 3; alert(a + b + c); "
        -> (p => false),

      "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;"
        -> (p => false),

      "var alert = function (i) {}; var a = 1; var b = 2; if (a <= b) { alert('ok'); } ;"
        -> (p => false),

      "var add = function (a, b) {return a + b}; var b = add(1, 2);"
        -> { p => println(p.program); true },

      "function (a, b) {return a - b};"
        -> { p => println(p.program); true }

    ) foreach { code =>
      try {
        val p = Parsing.parse(code._1)
        if (code._2(new ParsedData(p))) {
        } else {
          println("Fail:  " + code._1)
        }
      } catch {
        case a: Exception => a.printStackTrace(); println("Error: " + code)
      }
    }
  }
}

class ParsedData(
  val program: Program
)