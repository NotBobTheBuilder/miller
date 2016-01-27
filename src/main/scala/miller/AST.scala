package miller

object AST {

  case class Point(line: Int, col: Int)
  case class Pos(start: Point, end: Point)

  object Pos {
    val monoid = Pos(Point(0, 0), Point(0, 0))
  }

  sealed trait ASTNode {
    val pos: Pos = Pos.monoid
  }

  case class Program(statements: Seq[Statement])

  sealed trait Statement extends ASTNode {}

  case class Return(value: Expr) extends Statement
  case class Declare(name: String, value: Expr) extends Statement
  case class Assign(name: String, value: Expr) extends Statement
  case class While(cond: Expr, block: Seq[Statement]) extends Statement
  case class If(cond: Expr, block: Seq[Statement]) extends Statement
  case class IfElse(cond: Expr, tBlock: Seq[Statement], fBlock: Seq[Statement]) extends Statement

  sealed trait Expr extends Statement

  sealed trait Op extends Expr

  case class Add(lhs: Expr, rhs: Expr) extends Op
  case class Sub(lhs: Expr, rhs: Expr) extends Op
  case class Mul(lhs: Expr, rhs: Expr) extends Op
  case class Div(lhs: Expr, rhs: Expr) extends Op
  case class Mod(lhs: Expr, rhs: Expr) extends Op
  case class Lt(lhs: Expr, rhs: Expr) extends Op
  case class LtEq(lhs: Expr, rhs: Expr) extends Op
  case class Gt(lhs: Expr, rhs: Expr) extends Op
  case class GtEq(lhs: Expr, rhs: Expr) extends Op
  case class Eq(lhs: Expr, rhs: Expr) extends Op

  sealed trait Value extends Expr

  case class Ident(name: String) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int) extends Value
  case class LiteralStr(string: String) extends Value
  case class True() extends Value
  case class False() extends Value
  case class Null() extends Value
  case class Undefined() extends Value
  case class This() extends Value
  case class JSFunction(name: Option[String], params: Seq[String], block: Seq[Statement]) extends Value
  case class JSCall(f: Expr, ps: Seq[Expr]) extends Value

}