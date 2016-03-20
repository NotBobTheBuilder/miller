package miller

import miller.ASTf.Expr

import scala.io.Source.stdin

object Miller {
  def main(args: Array[String]): Unit = {
    val code = stdin.getLines().mkString("\n")

    val parsed = Parsing.parse(code)
    val checked = Inference.check(code)
    println(parsed)
    println(checked)
    println(parsed.stack.groupTypes)
    println(parsed.statements.lastOption match {
      case Some(e: Expr) => e.t
      case _ => "(Not an expression)"
    })
    // TODO : Generate buggy function call counterexamples
  }
}
