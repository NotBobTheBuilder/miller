package miller

import miller.ASTf._

object Inference {

  def check(input: String): String = {
    val (pass, errs) = test(input)
    if (pass) { "OK: " + input + "\n" } else { errs }
  }

  def test(input: String): (Boolean, String) = {
    val tree = Parsing.parse(input)
    val results = check(tree.statements, tree.stack)
    val pass = results.isEmpty

    pass -> (input + results.map { poserr =>
      poserr._1.position + "\n" + poserr._2
    }.mkString("\n", "\n", "\n"))
  }

  def check(ss: Seq[Statement], st: ScopeStack): Seq[(Position, String)] = {
    implicit val stack = st
    // 1 + ""; var a = ""; var b = 1; a + b; var add = function(x) { return x + 1 }; var mul = function (y, z) { return y * z };
    ss.flatMap {
      case Declare(es, pos)               => es.flatMap(e => checkOne(e._3, pos))
      case While(c, bs, pos)              => checkOne(c.t, c.pos) ++ check(bs, st)
      case If(c, bs, pos)                 => checkOne(c.t, c.pos) ++ check(bs, st)
      case IfElse(c, ts, fs, pos)         => checkOne(c.t, c.pos) ++ check(ts, st) ++ check(fs, st)
      case Return(e, pos)                 => checkOne(e.t, pos)
      case JSFunction(_, _, bs, t, pos)   => check(bs, st) ++ checkOne(t, pos)
      case e: Expr                        => checkOne(e.t, e.pos)
    }
  }

  def checkOne(a: InferredType, pos: Position): Seq[(Position, String)] = {
    a match {
      case NoInterErr(ts) => Seq(pos -> s"Type Error - incompatible types: ${ts.mkString(" | ")}")
      case BadArgsErr(f, e) => Seq(pos -> s"Function takes ${f.params.length}, called with ${e.length}")
      case _ => Seq()
    }
  }

}