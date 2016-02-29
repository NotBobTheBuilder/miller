import miller.ASTf.JSFunction
import miller.{IntersectT, ConstT, TFunction, Parsing}

object TypeInference {
  def main() = {
    Parsing.parse("function sub (a, b) { return a - b }").statements.head match {
      case f: JSFunction => f.t match {
        case ConstT(TFunction(Seq(IntersectT(n), IntersectT(p)), IntersectT(r))) =>
          println(n)
          println(p)
          println(r)
        case _ => println(f.t)
      }
    }
  }
}
