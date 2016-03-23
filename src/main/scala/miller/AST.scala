package miller

case class Position(startLine: Int, startCol: Int, endLine: Int, endCol: Int) extends Ordered[Position] {
  override def toString = s"l${startLine}c$startCol"
  def position = paddedPrefix("^" * (endCol - startCol))
  def paddedPrefix(s: String) = (" " * (startCol - 1)) + s

  def union(that: Position) = {
    val max = Set(this, that).max
    val min = Set(this, that).min
    Position(
      min.startLine,
      min.startCol,
      max.endLine,
      max.endCol
    )
  }

  def compare(that: Position) =
    implicitly[Ordering[(Int, Int)]] compare ((this.startLine, this.startCol), (that.startLine, that.startCol))
}

trait Pos {
  val pos: Position
}

object AST {

  sealed trait ASTNode extends Pos

  case class Program(statements: Seq[Statement], pos: Position) extends Pos

  sealed trait Statement extends ASTNode {}

  case class Return(value: Expr, pos: Position) extends Statement
  case class Declare(vars: Seq[(String, Option[Expr])], pos: Position) extends Statement
  case class While(cond: Expr, block: Seq[Statement], pos: Position) extends Statement
//  case class JsFor(cond: Expr, block: Seq[Statement], pos: Position) extends Statement
  case class JsForIn(ident: String, expr: Expr, block: Seq[Statement], pos: Position) extends Statement
  case class If(cond: Expr, block: Seq[Statement], pos: Position) extends Statement
  case class IfElse(cond: Expr, tBlock: Seq[Statement], fBlock: Seq[Statement], pos: Position) extends Statement

  sealed trait Expr extends Statement

  sealed trait Op extends Expr

  case class PostInc(exp: Expr, pos: Position) extends Op
  case class PostDec(exp: Expr, pos: Position) extends Op

  case class Not(exp: Expr, pos: Position) extends Op
  case class BitNot(exp: Expr, pos: Position) extends Op
  case class UAdd(exp: Expr, pos: Position) extends Op
  case class USub(exp: Expr, pos: Position) extends Op

  case class PreInc(exp: Expr, pos: Position) extends Op
  case class PreDec(exp: Expr, pos: Position) extends Op

  case class TypeOf(exp: Expr, pos: Position) extends Op
  case class Void(exp: Expr, pos: Position) extends Op
  case class Delete(exp: Expr, pos: Position) extends Op

  case class Mul(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Div(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Mod(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class Add(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Sub(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class LShift(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class RShift(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class URShift(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class Lt(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class LtEq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Gt(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class GtEq(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class In(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class InstanceOf(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class Eq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class NEq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class EEq(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class NEEq(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class BinAnd(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class BinXor(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class BinOr(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class And(lhs: Expr, rhs: Expr, pos: Position) extends Op
  case class Or(lhs: Expr, rhs: Expr, pos: Position) extends Op

  case class Ternary(cond: Expr, tBlock: Expr, fBlock: Expr, pos: Position) extends Op

  case class Assign(variable: Expr, value: Expr, pos: Position) extends Op
  case class AddEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class SubEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class DivEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class MulEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class ModEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class LShiftEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class RShiftEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class URShiftEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class BinAndEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class BinXorEq(variable: Expr, value: Expr, pos: Position) extends Op
  case class BinOrEq(variable: Expr, value: Expr, pos: Position) extends Op

  sealed trait Value extends Expr

  case class Ident(name: String, pos: Position) extends Value
  case class LiteralRegExp(chars: String, flags: String, pos: Position) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, pos: Position) extends Value
  case class LiteralStr(string: String, pos: Position) extends Value
  case class True(pos: Position) extends Value
  case class False(pos: Position) extends Value
  case class Null(pos: Position) extends Value
  case class Undefined(pos: Position) extends Value
  case class This(pos: Position) extends Value
  case class JsFunction(name: Option[String], params: Seq[String], block: Seq[Statement], pos: Position) extends Value
  case class JsCall(f: Expr, ps: Seq[Expr], pos: Position) extends Value
  case class Member(e: Expr, member: String, pos: Position) extends Value
  case class CompMem(e: Expr, prop: Expr, pos: Position) extends Value
  case class New(e: Expr, ps: Seq[Expr], pos: Position) extends Value
  case class CommaList(es: Seq[Expr], pos: Position) extends Value
  case class JsArray(e: Seq[Expr], pos: Position) extends Value
  case class JsObject(e: Map[String, Expr], pos: Position) extends Value
}