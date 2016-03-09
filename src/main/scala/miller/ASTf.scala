package miller

import scala.language.implicitConversions

case class Loan[T, U](l: T) {
  def to(f: (T)=>U) = f(l)
}

object ASTf {

  sealed trait ASTNode extends Pos

  case class Program(statements: Seq[Statement], stack: ScopeStack, pos: Position) extends Pos

  object Program {

    implicit def expr2f(exp: AST.Expr)(implicit st: ScopeStack): ASTf.Expr = {
      import Expr._
      exp match {
        case               AST.PostInc(e, p) => postInc(e, p)
        case               AST.PostDec(e, p) => postDec(e, p)

        case                   AST.Not(e, p) => not(e, p)
        case                AST.BitNot(e, p) => bitnot(e, p)
        case                  AST.UAdd(e, p) => uadd(e, p)
        case                  AST.USub(e, p) => usub(e, p)

        case                AST.PreInc(e, p) => preInc(e, p)
        case                AST.PreDec(e, p) => preDec(e, p)

        case                AST.TypeOf(e, p) => typeOf(e, p)
        case                  AST.Void(e, p) => void(e, p)
        case                AST.Delete(e, p) => delete(e, p)

        case          AST.Mul(lhs, rhs, pos) => mul(lhs, rhs, pos)
        case          AST.Div(lhs, rhs, pos) => div(lhs, rhs, pos)
        case          AST.Mod(lhs, rhs, pos) => mod(lhs, rhs, pos)

        case          AST.Add(lhs, rhs, pos) => add(lhs, rhs, pos)
        case          AST.Sub(lhs, rhs, pos) => sub(lhs, rhs, pos)

        case       AST.LShift(lhs, rhs, pos) => lshift(lhs, rhs, pos)
        case       AST.RShift(lhs, rhs, pos) => rshift(lhs, rhs, pos)
        case      AST.URShift(lhs, rhs, pos) => urshift(lhs, rhs, pos)

        case           AST.Lt(lhs, rhs, pos) => lt(lhs, rhs, pos)
        case         AST.LtEq(lhs, rhs, pos) => lteq(lhs, rhs, pos)
        case           AST.Gt(lhs, rhs, pos) => gt(lhs, rhs, pos)
        case         AST.GtEq(lhs, rhs, pos) => gteq(lhs, rhs, pos)

        case           AST.In(lhs, rhs, pos) => in(lhs, rhs, pos)
        case   AST.InstanceOf(lhs, rhs, pos) => instanceof(lhs, rhs, pos)

        case           AST.Eq(lhs, rhs, pos) => jsEq(lhs, rhs, pos)
        case          AST.NEq(lhs, rhs, pos) => neq(lhs, rhs, pos)
        case          AST.EEq(lhs, rhs, pos) => eeq(lhs, rhs, pos)
        case         AST.NEEq(lhs, rhs, pos) => neeq(lhs, rhs, pos)

        case       AST.BinAnd(lhs, rhs, pos) => binand(lhs, rhs, pos)
        case       AST.BinXor(lhs, rhs, pos) => binxor(lhs, rhs, pos)
        case        AST.BinOr(lhs, rhs, pos) => binor(lhs, rhs, pos)

        case          AST.And(lhs, rhs, pos) => and(lhs, rhs, pos)
        case           AST.Or(lhs, rhs, pos) => or(lhs, rhs, pos)

        case       AST.Assign(lhs, rhs, pos) => assign(lhs, rhs, pos)
        case        AST.AddEq(lhs, rhs, pos) => addeq(lhs, rhs, pos)
        case        AST.SubEq(lhs, rhs, pos) => subeq(lhs, rhs, pos)
        case        AST.DivEq(lhs, rhs, pos) => diveq(lhs, rhs, pos)
        case        AST.MulEq(lhs, rhs, pos) => muleq(lhs, rhs, pos)
        case        AST.ModEq(lhs, rhs, pos) => modeq(lhs, rhs, pos)
        case     AST.LShiftEq(lhs, rhs, pos) => lshifteq(lhs, rhs, pos)
        case     AST.RShiftEq(lhs, rhs, pos) => rshifteq(lhs, rhs, pos)
        case    AST.URShiftEq(lhs, rhs, pos) => urshifteq(lhs, rhs, pos)
        case     AST.BinAndEq(lhs, rhs, pos) => binandeq(lhs, rhs, pos)
        case     AST.BinXorEq(lhs, rhs, pos) => binxoreq(lhs, rhs, pos)
        case      AST.BinOrEq(lhs, rhs, pos) => binoreq(lhs, rhs, pos)

        case               AST.Ident(n, pos) => ident(st, n, pos)
        case  AST.LiteralRegExp(cs, fs, pos) => LiteralRegExp(cs, fs, pos)
        case       AST.LiteralNum(d, f, pos) => LiteralNum(d, f, pos)
        case          AST.LiteralStr(s, pos) => LiteralStr(s, pos)
        case                   AST.True(pos) => True(pos)
        case                  AST.False(pos) => False(pos)
        case                   AST.Null(pos) => Null(pos)
        case              AST.Undefined(pos) => Undefined(pos)
        case                   AST.This(pos) => This(pos)

        case         AST.Member(e, mem, pos) => Loan(expr2f(e)) to (e => Member(e, mem, e.t, pos))
        case       AST.CompMem(e, prop, pos) => Loan(expr2f(e)) to (e => CompMem(e, prop, e.t, pos))
        case             AST.JsArray(e, pos) => Loan(exprSeq2f(e)) to (e => JsArray(e, ConstT(TArray(ConstT(TUndefined))), pos))
        case            AST.JsObject(e, pos) => JsObject(Seq(), ConstT(TObject(Map())), pos)
        case             AST.New(e, ps, pos) => Loan(expr2f(e)) to (e => New(e, ps, e.t, pos))

        case  AST.Ternary(cond, tB, fB, pos) => Ternary(cond, tB, fB, tB.t intersect fB.t, pos)

        case          AST.CommaList(es, pos) => Loan(exprSeq2f(es)) to (es => CommaList(es, es.last.t, pos))

        case AST.JsFunction(n, ps, b, pos) =>
          val paramIds = st.pushScope(ps)
          val block: Seq[Statement] = b
          val tps = st.paramTypes
          val rt = st.returnType
          st.popScope()

          ASTf.JSFunction(n, paramIds, block, ConstT(TFunction(tps, rt)), pos)

        case AST.JsCall(ff, ps, pos) =>
          val f = expr2f(ff)
          JSCall(f, ps, applyCall(f.t, ps, st), pos) // TODO: this
      }
    }

    def applyCall(t: InferredType, ps: Seq[Expr], st: ScopeStack): InferredType = {
      implicit val sta = st
      t match {
        case ConstT(t: TFunction) => applyFunction(t, ps)
        case IntersectT(i) => applyCall(st.getGroupType(GroupID(i)), ps, st)
      }
    }

    def applyFunction(f: TFunction, ps: Seq[ASTf.Expr])(implicit st: ScopeStack): InferredType = {
      if (f.params.length != ps.length) {
        BadArgsErr(f, ps.map(_.t))
      } else {
        f.specialise(st)
        f.params.zip(ps).foldLeft(f.result) { (r, ps) =>
          val (expectedType, calledExpr) = ps

          expectedType intersect calledExpr.t match {
            case e: NoInterErr => e
            case _ => f.result
          }
        }
      }
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
      case AST.Declare(assignments, pos)   =>
        val fpairs = assignments.map { assignment =>
          val (ident, rhs) = assignment
          val frhs = rhs.map(expr2f)
          val t = frhs.map(_.t).getOrElse(AnyT)

          (st.declare(ident, t)._1, frhs, t)
        }
        ASTf.Declare(fpairs, pos)
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

  def ident(st: ScopeStack, n: String, pos: Position): Ident = {
    val vid = st.getId(n)
    Ident(vid, IntersectT(st.getGroup(vid.get).id), pos)
  }

  sealed trait Statement extends ASTNode {
    val pos: Position
  }

  case class Return(value: Expr, pos: Position) extends Statement
  case class Declare(vars: Seq[(VarID, Option[Expr], InferredType)], pos: Position) extends Statement

  case class While(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement

  case class If(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement

  case class IfElse(
    cond: Expr,
    trueBlock: Seq[Statement],
    falseBlock: Seq[Statement],
    pos: Position
  ) extends Statement

  sealed trait Expr extends Statement {
    val t: InferredType
  }

  object Expr {
    def uopp[T <: Expr](st: ScopeStack,
                        f: (Expr, InferredType, Position) => T,
                        opT: InferredType,
                        that: Expr,
                        pos: Position) = f(that, opT, pos)

    def interbinopp[T <: Expr](st: ScopeStack,
                               f: (Expr, Expr, InferredType, Position) => T,
                               opT: InferredType,
                               lhs: Expr,
                               rhs: Expr,
                               pos: Position) = {
      implicit val sta: ScopeStack = st

      f(lhs, rhs, lhs.t intersect rhs.t intersect opT, pos)
    }

    def constbinopp[T <: Expr](st: ScopeStack,
                               f: (Expr, Expr, InferredType, Position) => T,
                               opT: InferredType,
                               lhs: Expr,
                               rhs: Expr,
                               pos: Position) = {
      implicit val sta: ScopeStack = st

      f(lhs, rhs, opT, pos)
    }

    def assignopp[T <: Expr](st: ScopeStack,
                               f: (Expr, Expr, InferredType, Position) => T,
                               opT: InferredType,
                               lhs: Expr,
                               rhs: Expr,
                               pos: Position) = {
      implicit val sta: ScopeStack = st

      val tt = lhs.t intersect rhs.t intersect opT

      f(lhs, rhs, tt, pos)
    }

    def postInc(that: Expr, pos: Position)(implicit st: ScopeStack) =                 uopp(st, PostInc,   ConstT(TNumber), that, pos)
    def postDec(that: Expr, pos: Position)(implicit st: ScopeStack) =                 uopp(st, PostDec,   ConstT(TNumber), that, pos)

    def not(that: Expr, pos: Position)(implicit st: ScopeStack) =                     uopp(st, Not,       AnyT, that, pos)
    def bitnot(that: Expr, pos: Position)(implicit st: ScopeStack) =                  uopp(st, BitNot,    ConstT(TNumber), that, pos)
    def uadd(that: Expr, pos: Position)(implicit st: ScopeStack) =                    uopp(st, UAdd,      AnyT, that, pos)
    def usub(that: Expr, pos: Position)(implicit st: ScopeStack) =                    uopp(st, USub,      ConstT(TNumber), that, pos)

    def preInc(that: Expr, pos: Position)(implicit st: ScopeStack) =                  uopp(st, PreInc,    ConstT(TNumber), that, pos)
    def preDec(that: Expr, pos: Position)(implicit st: ScopeStack) =                  uopp(st, PreDec,    ConstT(TNumber), that, pos)

    def typeOf(that: Expr, pos: Position)(implicit st: ScopeStack) =                  uopp(st, TypeOf,    AnyT, that, pos)
    def void(that: Expr, pos: Position)(implicit st: ScopeStack) =                    uopp(st, Void,      AnyT, that, pos)
    def delete(that: Expr, pos: Position)(implicit st: ScopeStack) =                  uopp(st, Delete,    AnyT, that, pos)

    def div(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           interbinopp(st, Div,        ConstT(TNumber), lhs, rhs, pos)
    def mul(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           interbinopp(st, Mul,        ConstT(TNumber), lhs, rhs, pos)
    def mod(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           interbinopp(st, Mod,        ConstT(TNumber), lhs, rhs, pos)

    def add(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           interbinopp(st, Add,        SetT(Set(TString, TNumber)), lhs, rhs, pos)
    def sub(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           interbinopp(st, Sub,        ConstT(TNumber), lhs, rhs, pos)

    def lshift(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =        interbinopp(st, Div,        ConstT(TNumber), lhs, rhs, pos)
    def rshift(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =        interbinopp(st, Mul,        ConstT(TNumber), lhs, rhs, pos)
    def urshift(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =       interbinopp(st, Mod,        ConstT(TNumber), lhs, rhs, pos)

    def lt(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =            interbinopp(st, Lt,         ConstT(TNumber), lhs, rhs, pos)
    def lteq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =          interbinopp(st, LtEq,       ConstT(TNumber), lhs, rhs, pos)
    def gt(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =            interbinopp(st, Gt,         ConstT(TNumber), lhs, rhs, pos)
    def gteq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =          interbinopp(st, GtEq,       ConstT(TNumber), lhs, rhs, pos)

    def in(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =            constbinopp(st, In,         ConstT(TBoolean), lhs, rhs, pos)
    def instanceof(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =    constbinopp(st, InstanceOf, ConstT(TBoolean), lhs, rhs, pos)

    def jsEq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =          constbinopp(st, Eq,         ConstT(TBoolean), lhs, rhs, pos)
    def neq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           constbinopp(st, NEq,        ConstT(TBoolean), lhs, rhs, pos)
    def eeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           constbinopp(st, EEq,        ConstT(TBoolean), lhs, rhs, pos)
    def neeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =          constbinopp(st, NEEq,       ConstT(TBoolean), lhs, rhs, pos)

    def binand(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =        interbinopp(st, BinAnd,     ConstT(TNumber), lhs, rhs, pos)
    def binxor(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =        interbinopp(st, BinXor,     ConstT(TNumber), lhs, rhs, pos)
    def binor(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         interbinopp(st, BinOr,      ConstT(TNumber), lhs, rhs, pos)

    def and(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =           constbinopp(st, And,        ConstT(TBoolean), lhs, rhs, pos)
    def or(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =            constbinopp(st, Or,         ConstT(TBoolean), lhs, rhs, pos)

    def assign(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =        assignopp(st, Assign,     AnyT, lhs, rhs, pos)
    def addeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         assignopp(st, AddEq,      SetT(Set(TNumber, TString)), lhs, rhs, pos)
    def subeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         assignopp(st, SubEq,      ConstT(TNumber), lhs, rhs, pos)
    def diveq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         assignopp(st, DivEq,      ConstT(TNumber), lhs, rhs, pos)
    def muleq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         assignopp(st, MulEq,      ConstT(TNumber), lhs, rhs, pos)
    def modeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =         assignopp(st, ModEq,      ConstT(TNumber), lhs, rhs, pos)
    def lshifteq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =      assignopp(st, LShiftEq,   ConstT(TNumber), lhs, rhs, pos)
    def rshifteq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =      assignopp(st, RShiftEq,   ConstT(TNumber), lhs, rhs, pos)
    def urshifteq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =     assignopp(st, URShiftEq,  ConstT(TNumber), lhs, rhs, pos)
    def binandeq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =      assignopp(st, BinAndEq,   ConstT(TNumber), lhs, rhs, pos)
    def binxoreq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =      assignopp(st, BinXorEq,   ConstT(TNumber), lhs, rhs, pos)
    def binoreq(lhs: Expr, rhs: Expr, pos: Position)(implicit st: ScopeStack) =       assignopp(st, BinOrEq,    ConstT(TNumber), lhs, rhs, pos)
  }

  sealed trait Op extends Expr

  case class PostInc(exp: Expr, t: InferredType, pos: Position) extends Op
  case class PostDec(exp: Expr, t: InferredType, pos: Position) extends Op

  case class Not(exp: Expr, t: InferredType, pos: Position) extends Op
  case class BitNot(exp: Expr, t: InferredType, pos: Position) extends Op
  case class UAdd(exp: Expr, t: InferredType, pos: Position) extends Op
  case class USub(exp: Expr, t: InferredType, pos: Position) extends Op

  case class PreInc(exp: Expr, t: InferredType, pos: Position) extends Op
  case class PreDec(exp: Expr, t: InferredType, pos: Position) extends Op

  case class TypeOf(exp: Expr, t: InferredType, pos: Position) extends Op
  case class Void(exp: Expr, t: InferredType, pos: Position) extends Op
  case class Delete(exp: Expr, t: InferredType, pos: Position) extends Op

  case class Mul(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Div(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Mod(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class Add(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Sub(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class LShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class RShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class URShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class Lt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class LtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Gt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class GtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class In(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class InstanceOf(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class Eq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class NEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class EEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class NEEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class BinAnd(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class BinXor(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class BinOr(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class And(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op
  case class Or(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends Op

  case class Ternary(cond: Expr, tBlock: Expr, fBlock: Expr, t: InferredType, pos: Position) extends Op

  case class Assign(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class AddEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class SubEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class DivEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class MulEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class ModEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class LShiftEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class RShiftEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class URShiftEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class BinAndEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class BinXorEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op
  case class BinOrEq(variable: Expr, value: Expr, t: InferredType, pos: Position) extends Op

  sealed trait Value extends Expr

  case class Ident(varID: Option[VarID], t: InferredType, pos: Position) extends Value
  case class LiteralRegExp(chars: String, flags: String, pos: Position, t: InferredType = ConstT(TRegExp)) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, pos: Position, t: InferredType = ConstT(TNumber)) extends Value
  case class LiteralStr(string: String, pos: Position, t: InferredType = ConstT(TString)) extends Value
  case class True(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class False(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class Null(pos: Position, t: InferredType = ConstT(TNull)) extends Value
  case class Undefined(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value
  case class This(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value
  case class JSFunction(name: Option[String], params: Seq[VarID], block: Seq[Statement], t: InferredType, pos: Position) extends Value
  case class JSCall(f: Expr, ps: Seq[Expr], t: InferredType, pos: Position) extends Value
  case class Member(e: Expr, member: String, t: InferredType, pos: Position) extends Value
  case class CompMem(e: Expr, prop: Expr, t: InferredType, pos: Position) extends Value
  case class New(e: Expr, ps: Seq[Expr], t: InferredType, pos: Position) extends Value
  case class CommaList(es: Seq[Expr], t: InferredType, pos: Position) extends Value
  case class JsObject(e: Seq[(String, Expr)], t: InferredType, pos: Position) extends Value
  case class JsArray(e: Seq[Expr], t: InferredType, pos: Position) extends Value
}
