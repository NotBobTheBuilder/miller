package miller

object ASTf {

  case class Point(line: Int, col: Int)
  case class Pos(start: Point, end: Point)

  object Pos {
    val monoid = Pos(Point(0, 0), Point(0, 0))
  }

  sealed trait ASTNode {
    val pos: Pos = Pos.monoid
  }

  sealed trait Block

  case class Program(statements: Seq[Statement], stack: ScopeStack) extends Block

  object Program {

    def tupled[T, U, V](t: U => V, u: T => Option[U]) = { n: T => t(u(n).get) }
    def tupledTree[T, V](t: ((ASTf.Expr, ASTf.Expr)) => V, u: T => Option[(AST.Expr, AST.Expr)])(implicit st: ScopeStack) = { n: T =>
      t(u(n).get match {
        case a: (AST.Expr, AST.Expr) => (expr2f(a._1), expr2f(a._2))
      })
    }

    implicit def op2f(o: AST.Op)(implicit st: ScopeStack): ASTf.Op = o match {
      case AST.Add(lhs, rhs) =>  ASTf.Add(lhs, rhs, lhs.t intersect rhs.t intersect SetT(Set(TNumber, TString)))
      case AST.Sub(lhs, rhs) =>  ASTf.Sub(lhs, rhs, lhs.t intersect rhs.t intersect ConstT(TNumber))
      case AST.Mul(lhs, rhs) =>  ASTf.Mul(lhs, rhs, lhs.t intersect rhs.t intersect ConstT(TNumber))
      case AST.Div(lhs, rhs) =>  ASTf.Div(lhs, rhs, lhs.t intersect rhs.t intersect ConstT(TNumber))
      case AST.Mod(lhs, rhs) =>  ASTf.Mod(lhs, rhs, lhs.t intersect rhs.t intersect ConstT(TNumber))

      // TODO: Check types of subexprs
      case AST.Gt(lhs, rhs) =>    ASTf.Gt(lhs, rhs, ConstT(TBoolean))
      case AST.Lt(lhs, rhs) =>    ASTf.Lt(lhs, rhs, ConstT(TBoolean))
      case AST.Eq(lhs, rhs) =>    ASTf.Eq(lhs, rhs, ConstT(TBoolean))
      case AST.GtEq(lhs, rhs) =>  ASTf.GtEq(lhs, rhs, ConstT(TBoolean))
      case AST.LtEq(lhs, rhs) =>  ASTf.LtEq(lhs, rhs, ConstT(TBoolean))
    }
    //

    implicit def value2f(v: AST.Value)(implicit st: ScopeStack): ASTf.Value = v match {
      case AST.Ident(n)             => ASTf.Ident(st.getId(n), st.getId(n).map(p => VarT(p)).getOrElse(ConstT(TUndefined): InferredType))
      case AST.LiteralNum(d, f)     => ASTf.LiteralNum(d, f, ConstT(TNumber))
      case AST.LiteralStr(s)        => ASTf.LiteralStr(s, ConstT(TString))
      case AST.True()               => ASTf.True()
      case AST.False()              => ASTf.False()
      case AST.Null()               => ASTf.Null()
      case AST.Undefined()          => ASTf.Undefined()
      case AST.This()               => ASTf.This()
      case AST.JSFunction(n, ps, b) =>
        st.pushScope(ps)
        val block: Seq[Statement] = b
        val tps = st.paramTypes
        val rt = st.returnType
        st.popScope()

        ASTf.JSFunction(n, ps, block, ConstT(TFunction(tps, rt)))

      case AST.JSCall(f, ps)        => ASTf.JSCall(f, ps, f.t match {
        case VarT(n) => ConstT(TUndefined) // TODO: figure out this bit
          // probably gonna involve some refactoring to check constraints on parameters & function parameter vargroups
      })
    }

    implicit def expr2f(e: AST.Expr)(implicit st: ScopeStack): ASTf.Expr = e match {
      case o: AST.Op => o
      case v: AST.Value => v
    }

    implicit def exprSeq2f(s: Seq[AST.Expr])(implicit st: ScopeStack): Seq[ASTf.Expr] = s.map(expr2f)
    implicit def exprpair2f(e: (AST.Expr, AST.Expr))(implicit st: ScopeStack): (ASTf.Expr, ASTf.Expr) = e match {
      case e: (AST.Expr, AST.Expr) => (e._1, e._2)
    }

    implicit def statement2f(s: AST.Statement)(implicit st: ScopeStack): ASTf.Statement = s match {
      case e: AST.Expr              => e
      case AST.Return(v)            => st.ret(v.t); ASTf.Return(v)
      case AST.Declare(n, e)        => ASTf.Declare(st.declare(n, e.t), e)
      case AST.Assign(n, e)         => ASTf.Assign(st.getId(n), e)
      case AST.If(cond, block)      => ASTf.If(cond, block)
      case AST.IfElse(cond, tb, fb) => ASTf.IfElse(cond, tb, fb)
      case AST.While(cond, b)       => ASTf.While(cond, b)
    }
    implicit def statSeq2f(s: Seq[AST.Statement])(implicit st: ScopeStack): Seq[ASTf.Statement] = s.map(statement2f)
    implicit def program2f(p: AST.Program): ASTf.Program = {
      implicit val scope = new ScopeStack()
      Program(p.statements, scope)
    }
  }

  sealed trait Statement extends ASTNode {}

  case class Return(value: Expr) extends Statement
  case class Declare(ident: Int, value: Expr) extends Statement
  case class Assign(ident: Option[Int], value: Expr) extends Statement

  case class While(
    cond: Expr,
    block: Seq[Statement]
  ) extends Statement with Block

  case class If(
    cond: Expr,
    block: Seq[Statement]
  ) extends Statement with Block

  case class IfElse(
    cond: Expr,
    trueBlock: Seq[Statement],
    falseBlock: Seq[Statement]
  ) extends Statement with Block

  sealed trait Expr extends Statement {
    val t: InferredType
  }

  sealed trait Op extends Expr

  case class Add(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Sub(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Mul(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Div(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Mod(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Lt(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class LtEq(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Gt(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class GtEq(lhs: Expr, rhs: Expr, t: InferredType) extends Op
  case class Eq(lhs: Expr, rhs: Expr, t: InferredType) extends Op

  sealed trait Value extends Expr

  case class Ident(varID: Option[Int], t: InferredType) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, t: InferredType = ConstT(TNumber)) extends Value
  case class LiteralStr(string: String, t: InferredType = ConstT(TNumber)) extends Value
  case class True(t: InferredType = ConstT(TBoolean)) extends Value
  case class False(t: InferredType = ConstT(TBoolean)) extends Value
  case class Null(t: InferredType = ConstT(TNull)) extends Value
  case class Undefined(t: InferredType = ConstT(TUndefined)) extends Value
  case class This(t: InferredType = ConstT(TUndefined)) extends Value
  case class JSFunction(
    name: Option[String],
    params: Seq[String],
    block: Seq[Statement],
    t: InferredType
  ) extends Value with Block
  case class JSCall(f: Expr, ps: Seq[Expr], t: InferredType) extends Value

}