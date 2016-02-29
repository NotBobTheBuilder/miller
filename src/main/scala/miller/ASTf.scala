package miller

import scala.language.implicitConversions

object ASTf {

  sealed trait ASTNode extends Pos

  sealed trait Block

  case class Program(statements: Seq[Statement], stack: ScopeStack, pos: Position) extends Block with Pos

  object Program {

    def tupled[T, U, V](t: U => V, u: T => Option[U]) = { n: T => t(u(n).get) }
    def tupledTree[T, V](t: ((ASTf.Expr, ASTf.Expr)) => V, u: T => Option[(AST.Expr, AST.Expr)])(implicit st: ScopeStack) = { n: T =>
      t(u(n).get match {
        case a: (AST.Expr, AST.Expr) => (expr2f(a._1), expr2f(a._2))
      })
    }

    def op2f(o: AST.Op)(implicit st: ScopeStack): ASTf.Op = o match {
      case AST.Add(lhs, rhs, pos) =>
        val lhs1 = expr2f(lhs)
        val rhs1 = expr2f(rhs)
        ASTf.Add(lhs1, rhs1, lhs1.t intersect rhs1.t intersect SetT(Set(TNumber, TString)), pos)
      case AST.Sub(lhs, rhs, pos) =>
        val lhs1 = expr2f(lhs)
        val rhs1 = expr2f(rhs)
        ASTf.Sub(lhs1, rhs1, lhs1.t intersect rhs1.t intersect ConstT(TNumber), pos)
      case AST.Mul(lhs, rhs, pos) =>
        val lhs1 = expr2f(lhs)
        val rhs1 = expr2f(rhs)
        ASTf.Mul(lhs1, rhs1, lhs1.t intersect rhs1.t intersect ConstT(TNumber), pos)
      case AST.Div(lhs, rhs, pos) =>
        val lhs1 = expr2f(lhs)
        val rhs1 = expr2f(rhs)
        ASTf.Div(lhs1, rhs1, lhs1.t intersect rhs1.t intersect ConstT(TNumber), pos)
      case AST.Mod(lhs, rhs, pos) =>
        val lhs1 = expr2f(lhs)
        val rhs1 = expr2f(rhs)
        ASTf.Mod(lhs1, rhs1, lhs1.t intersect rhs1.t intersect ConstT(TNumber), pos)

      case AST.Gt(lhs, rhs, pos) =>    ASTf.Gt(lhs, rhs, ConstT(TBoolean), pos)
      case AST.Lt(lhs, rhs, pos) =>    ASTf.Lt(lhs, rhs, ConstT(TBoolean), pos)
      case AST.Eq(lhs, rhs, pos) =>    ASTf.Eq(lhs, rhs, ConstT(TBoolean), pos)
      case AST.GtEq(lhs, rhs, pos) =>  ASTf.GtEq(lhs, rhs, ConstT(TBoolean), pos)
      case AST.LtEq(lhs, rhs, pos) =>  ASTf.LtEq(lhs, rhs, ConstT(TBoolean), pos)
    }

    def value2f(v: AST.Value)(implicit st: ScopeStack): ASTf.Value = v match {
      case AST.Ident(n, pos)              => ASTf.Ident(st.getId(n), st.getId(n).map(p => VarT(p)).getOrElse(ConstT(TUndefined): InferredType), pos)
      case AST.LiteralNum(d, f, pos)      => ASTf.LiteralNum(d, f, pos)
      case AST.LiteralStr(s, pos)         => ASTf.LiteralStr(s, pos)
      case AST.True(pos)                  => ASTf.True(pos)
      case AST.False(pos)                 => ASTf.False(pos)
      case AST.Null(pos)                  => ASTf.Null(pos)
      case AST.Undefined(pos)             => ASTf.Undefined(pos)
      case AST.This(pos)                  => ASTf.This(pos)
      case AST.JSFunction(n, ps, b, pos)  =>
        val paramIds = st.pushScope(ps)
        val block: Seq[Statement] = b
        val tps = st.paramTypes
        val rt = st.returnType
        st.popScope()

        ASTf.JSFunction(n, paramIds, block, ConstT(TFunction(tps, rt)), pos)

      case AST.JSCall(f, ps, pos)         => ASTf.JSCall(f, ps, f.t match {
        case VarT(n) => st.getType(n) match {
          case ConstT(t: TFunction) => applyFunction(t, ps)
          case _ => NoInterErr(Set(st.getType(n), ConstT(TFunction(Seq(), AnyT)))) // TODO: Handle this
        }
      }, pos)
    }

    def applyFunction(f: TFunction, ps: Seq[ASTf.Expr])(implicit st: ScopeStack): InferredType = {
      if (f.params.length != ps.length) {
        BadArgsErr(f, ps.map(_.t))
      } else {
        f.specialise(st)
        f.params.map(p => st.getGroupType(p.vs)).zip(ps).foldLeft(f.result) { (r, ps) =>
          val (expectedType, calledType) = ps
          if (calledType.t canSatisfy expectedType) {
            f.result
          } else {
            NoInterErr(Set(expectedType, calledType.t))
          }
        }
      }
    }

    implicit def expr2f(e: AST.Expr)(implicit st: ScopeStack): ASTf.Expr = e match {
      case o: AST.Op => op2f(o)
      case v: AST.Value => value2f(v)
    }

    implicit def exprSeq2f(s: Seq[AST.Expr])(implicit st: ScopeStack): Seq[ASTf.Expr] = s.map(expr2f)
    implicit def exprPair2f(e: (AST.Expr, AST.Expr))(implicit st: ScopeStack): (ASTf.Expr, ASTf.Expr) = e match {
      case e: (AST.Expr, AST.Expr) => (e._1, e._2)
    }

    implicit def statement2f(s: AST.Statement)(implicit st: ScopeStack): ASTf.Statement = s match {
      case e: AST.Expr                    => e
      case AST.Return(v, pos)             =>
        // TODO double check correct IntersectT value is set here
        // Parameter IntersectT doesn't seem to line up in a subtree like in
        // function (a, b) { return a - b }
        val e = expr2f(v)
        st.ret(e.t); ASTf.Return(e, pos)
      case AST.Declare(n, Some(e), pos)   =>
        val v = expr2f(e)
        ASTf.Declare(st.declare(n, v.t), v, pos)
      case AST.Declare(n, None, pos)      => ASTf.Declare(st.declare(n), Undefined(pos), pos) //TODO
//      case AST.Assign(n, e, pos)          => ASTf.Assign(st.getId(n), e, pos)
      case AST.Assign(n, e, pos)          => ASTf.Assign(None, e, pos) // TODO
      case AST.If(cond, block, pos)       => ASTf.If(cond, block, pos)
      case AST.IfElse(cond, tb, fb, pos)  => ASTf.IfElse(cond, tb, fb, pos)
      case AST.While(cond, b, pos)        => ASTf.While(cond, b, pos)
    }
    implicit def statSeq2f(s: Seq[AST.Statement])(implicit st: ScopeStack): Seq[ASTf.Statement] = s.map(statement2f)

    def program2f(p: AST.Program): ASTf.Program = {
      implicit val scope = new ScopeStack()
      Program(p.statements, scope, p.pos)
    }
  }

  sealed trait Statement extends ASTNode {}

  case class Return(value: Expr, pos: Position) extends Statement
  case class Declare(ident: Int, value: Expr, pos: Position) extends Statement
  case class Assign(ident: Option[Int], value: Expr, pos: Position) extends Statement

  case class While(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement with Block

  case class If(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement with Block

  case class IfElse(
    cond: Expr,
    trueBlock: Seq[Statement],
    falseBlock: Seq[Statement],
    pos: Position
  ) extends Statement with Block

  sealed trait Expr extends Statement {
    val t: InferredType
  }

  sealed trait Op extends Expr

  case class Add(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Sub(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Mul(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Div(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Mod(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Lt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class LtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Gt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class GtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Eq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  sealed trait Value extends Expr

  case class Ident(varID: Option[Int], t: InferredType, pos: Position) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, pos: Position, t: InferredType = ConstT(TNumber)) extends Value
  case class LiteralStr(string: String, pos: Position, t: InferredType = ConstT(TString)) extends Value
  case class True(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class False(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class Null(pos: Position, t: InferredType = ConstT(TNull)) extends Value
  case class Undefined(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value
  case class This(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value
  case class JSCall(f: Expr, ps: Seq[Expr], t: InferredType, pos: Position) extends Value
  case class JSFunction(
    name: Option[String],
    params: Seq[Int],
    block: Seq[Statement],
    t: InferredType,
    pos: Position
  ) extends Value with Block
}