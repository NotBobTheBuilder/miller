package miller

case class Position(startLine: Int, startCol: Int, endLine: Int, endCol: Int) {
  override def toString = s"l${startLine}c$startCol - l${endLine}c$endCol"
  def position = (" " * (startCol - 1)) + ("^" * (endCol - startCol))
}

trait Pos {
  val pos: Position
}

object AST {

  sealed trait ASTNode extends Pos

  case class Program(statements: Seq[Statement], pos: Position) extends Pos

  sealed trait Statement extends ASTNode {}

  case class Return(value: Expr, pos: Position) extends Statement
  case class Declare(name: String, value: Option[Expr], pos: Position) extends Statement
  case class Assign(name: String, value: Expr, pos: Position) extends Statement
  case class While(cond: Expr, block: Seq[Statement], pos: Position) extends Statement
  case class If(cond: Expr, block: Seq[Statement], pos: Position) extends Statement
  case class IfElse(cond: Expr, tBlock: Seq[Statement], fBlock: Seq[Statement], pos: Position) extends Statement

  sealed trait Expr extends Statement

  sealed trait Op extends Expr

  case class Add(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Sub(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Mul(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Div(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Mod(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Lt(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class LtEq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Gt(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class GtEq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Eq(lhs: Expr, rhs: Expr, pos: Position) extends Op

  sealed trait Value extends Expr

  case class Ident(name: String, pos: Position) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, pos: Position) extends Value
  case class LiteralStr(string: String, pos: Position) extends Value
  case class True(pos: Position) extends Value
  case class False(pos: Position) extends Value
  case class Null(pos: Position) extends Value
  case class Undefined(pos: Position) extends Value
  case class This(pos: Position) extends Value
  case class JSFunction(name: Option[String], params: Seq[String], block: Seq[Statement], pos: Position) extends Value
  case class JSCall(f: Expr, ps: Seq[Expr], pos: Position) extends Value

}