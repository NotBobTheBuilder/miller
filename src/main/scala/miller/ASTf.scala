package miller

import scala.language.implicitConversions

case class Loan[T, U](l: T) {
  def to(f: (T)=>U) = f(l)
}

object ASTf {

  sealed trait ASTNode extends Pos {
    type TypeLines = Seq[(Position, InferredType)]

    def typeAnnotations(m: Map[Int, TypeLines]): Map[Int, TypeLines]
    def addAnnotation(m: Map[Int, TypeLines], pos: Position, t: InferredType) = m ++ (pos.startLine to pos.endLine).map(l => l -> ((pos, t) +: m(l)))
  }

  case class Program(statements: Seq[Statement], stack: ScopeStack, pos: Position) extends Pos {
    val typeAnnotations =
      statements
        .foldLeft(Map[Int, ASTNode#TypeLines]() withDefaultValue Seq[(Position, InferredType)]())((types, statement) => statement.typeAnnotations(types))

    def annotateSource(source: Seq[String]): String = {
      source.zipWithIndex.flatMap({
        case (s, r) => typeAnnotations(r + 1).flatMap {
          case (ps, t) => Seq(s, ps.position, ps.paddedPrefix(t.serialize(stack)))
        }
      }).mkString("\n")
    }

  }

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

        case         AST.Member(e, mem, pos) => member(e, mem, pos)
        case       AST.CompMem(e, prop, pos) => compMem(e, prop, pos)
        case             AST.JsArray(e, pos) => litArray(exprSeq2f(e), pos)
        case            AST.JsObject(e, pos) => litObject(e.map(e => e._1 -> expr2f(e._2)), pos)
        case             AST.New(e, ps, pos) => Loan(expr2f(e)) to (e => New(e, ps, e.t, pos))

        case  AST.Ternary(cond, tB, fB, pos) => Ternary(cond, tB, fB, tB.t intersect fB.t, pos)

        case          AST.CommaList(es, pos) => Loan(exprSeq2f(es)) to (es => CommaList(es, es.last.t, pos))

        case AST.JsFunction(n, ps, b, pos) =>
          val paramIds = st.pushScope(ps)
          val block: Seq[Statement] = b
          val tps = st.paramTypes
          val rt = st.returnType
          st.popScope()

          rt match {
            case e: TypeError => ASTf.JsFunction(n, paramIds, block, e, pos)
            case _ => ASTf.JsFunction(n, paramIds, block, ConstT(TFunction(tps, rt)), pos)
          }

        case AST.JsCall(ff, pps, pos) =>
          val f = expr2f(ff)
          val ps = exprSeq2f(pps)
          val resultType = ps.collectFirst({
            case e: Expr if e.t.isInstanceOf[TypeError] => e.t
          }).getOrElse(applyCall(f.t, ps, st))

          JsCall(f, ps, resultType, pos)
      }
    }

    def applyCall(t: InferredType, ps: Seq[Expr], st: ScopeStack): InferredType = {
      implicit val sta = st
      t match {
        case ConstT(t: TFunction) => t.callwith(ps)
        case IntersectT(i) => applyCall(st.getGroupType(GroupID(i)), ps, st)
        case other => other
      }
    }

    implicit def exprSeq2f(s: Seq[AST.Expr])(implicit st: ScopeStack): Seq[ASTf.Expr] = s.map(expr2f)
    implicit def exprPair2f(e: (AST.Expr, AST.Expr))(implicit st: ScopeStack): (ASTf.Expr, ASTf.Expr) = (e._1, e._2)

    implicit def statement2f(s: AST.Statement)(implicit st: ScopeStack): ASTf.Statement = s match {
      case e: AST.Expr                    => e
      case AST.Return(v, pos)             =>
        val e = expr2f(v)
        st.ret(e.t)
        ASTf.Return(e, pos)

      case AST.Declare(assignments, pos)   =>
        val fpairs = assignments.map { case (ident, rhs) =>
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

  def litObject(es: Map[String, Expr], pos: Position)(implicit st: ScopeStack): ASTf.JsObject = {
    JsObject(es, ConstT(TObject(collection.mutable.Map(es.map(e => e._1 -> e._2.t).toSeq: _*))), pos)
  }

  def litArray(es: Seq[Expr], pos: Position)(implicit st: ScopeStack): ASTf.JsArray = {
    val arrT = es.foldLeft(AnyT: InferredType)(_ intersect _.t) match {
      case e: TypeError => e
      case t => ConstT(TArray(t))
    }
    JsArray(es, arrT, pos)
  }

  def member(exp: Expr, prop: String, pos: Position)(implicit st: ScopeStack): ASTf.Member = {
    val propertyT = exp.t.actualT match {
      case ConstT(t: TObject) => t.properties.getOrElse(prop, NotAProperty(t, prop))
      case ConstT(t) => t.properties.getOrElse(prop, ConstT(TUndefined))
    }
    Member(exp, prop, propertyT, pos)
  }

  def compMem(exp: Expr, prop: Expr, pos: Position)(implicit st: ScopeStack): ASTf.CompMem = {
    val t = exp.t.actualT match {
      case ConstT(t: TArray) => t.t
      case _ => ConstT(TUndefined)
    }
    CompMem(exp, prop, t, pos)
  }

  sealed trait Statement extends ASTNode {
    val pos: Position
  }

  case class Return(value: Expr, pos: Position) extends Statement {
    def typeAnnotations(m: Map[Int, TypeLines]) = value.typeAnnotations(m)
  }
  case class Declare(vars: Seq[(VarID, Option[Expr], InferredType)], pos: Position) extends Statement {
    def typeAnnotations(m: Map[Int, TypeLines]) = vars.foldLeft(m)((types, stmt) => stmt._2 match {
      case None => types
      case Some(e) => e.typeAnnotations(types)
    })
  }

  case class While(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement {
    def typeAnnotations(m: Map[Int, TypeLines]) = (cond +: block).foldLeft(m)((types, stmt) => stmt.typeAnnotations(types))
  }

  case class If(
    cond: Expr,
    block: Seq[Statement],
    pos: Position
  ) extends Statement {
    def typeAnnotations(m: Map[Int, TypeLines]) = (cond +: block).foldLeft(m)((types, stmt) => stmt.typeAnnotations(types))
  }

  case class IfElse(
    cond: Expr,
    trueBlock: Seq[Statement],
    falseBlock: Seq[Statement],
    pos: Position
  ) extends Statement {
    def typeAnnotations(m: Map[Int, TypeLines]) = (cond +: (trueBlock ++ falseBlock)).foldLeft(m)((types, stmt) => stmt.typeAnnotations(types))
  }

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

      def getMember(m: Member): InferredType = {
        m.e.t.actualT match {
          case ConstT(t: TObject) =>
            // TODO check rhs.t canSatisfy lhs.t || lhs.t is undefined
            t.properties += m.member -> rhs.t
            rhs.t
          case ConstT(t: TArray) =>
            t.properties.getOrElse(m.member, t.t)
        }
      }

      def getCompMember(c: CompMem): InferredType = {
        c.e.t match {
          case ConstT(a: TArray) => a.t
          case other => ConstT(TUndefined)
        }
      }

      val lhst = lhs match {
        case i: Ident => i.t
        case m: Member => getMember(m)
        case c: CompMem => getCompMember(c)
        case other => NotAssignableErr(lhs.t, rhs.t)
      }

      f(lhs, rhs, lhst intersect rhs.t intersect opT, pos)
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

  sealed trait UOp extends Op {
    val exp: Expr
    def typeAnnotations(m: Map[Int, TypeLines]) = exp.typeAnnotations(addAnnotation(m, pos, t))
  }

  sealed trait BinOp extends Op {
    val lhs: Expr
    val rhs: Expr
    def typeAnnotations(m: Map[Int, TypeLines]) = Seq(lhs, rhs).foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }

  case class PostInc(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class PostDec(exp: Expr, t: InferredType, pos: Position) extends UOp

  case class Not(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class BitNot(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class UAdd(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class USub(exp: Expr, t: InferredType, pos: Position) extends UOp

  case class PreInc(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class PreDec(exp: Expr, t: InferredType, pos: Position) extends UOp

  case class TypeOf(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class Void(exp: Expr, t: InferredType, pos: Position) extends UOp
  case class Delete(exp: Expr, t: InferredType, pos: Position) extends UOp

  case class Mul(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class Div(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class Mod(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class Add(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class Sub(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class LShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class RShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class URShift(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class Lt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class LtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class Gt(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class GtEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class In(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class InstanceOf(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class Eq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class NEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class EEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class NEEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class BinAnd(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class BinXor(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class BinOr(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class And(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class Or(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class Assign(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class AddEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class SubEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class DivEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class MulEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class ModEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class LShiftEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class RShiftEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class URShiftEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class BinAndEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class BinXorEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp
  case class BinOrEq(lhs: Expr, rhs: Expr, t: InferredType, pos: Position) extends BinOp

  case class Ternary(cond: Expr, tBlock: Expr, fBlock: Expr, t: InferredType, pos: Position) extends Op {
    def typeAnnotations(m: Map[Int, TypeLines]) = Seq(cond, tBlock, fBlock).foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }

  sealed trait Value extends Expr {
    def typeAnnotations(m: Map[Int, TypeLines]) = addAnnotation(m, pos, t)
  }

  case class Ident(varID: Option[VarID], t: InferredType, pos: Position) extends Value
  case class LiteralRegExp(chars: String, flags: String, pos: Position, t: InferredType = ConstT(TRegExp)) extends Value
  case class LiteralNum(decPart: Int, fracPart: Int, pos: Position, t: InferredType = ConstT(TNumber)) extends Value
  case class LiteralStr(string: String, pos: Position, t: InferredType = ConstT(TString)) extends Value
  case class True(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class False(pos: Position, t: InferredType = ConstT(TBoolean)) extends Value
  case class Null(pos: Position, t: InferredType = ConstT(TNull)) extends Value
  case class Undefined(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value
  case class This(pos: Position, t: InferredType = ConstT(TUndefined)) extends Value

  case class JsFunction(name: Option[String], params: Seq[VarID], block: Seq[Statement], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = block.foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
  case class JsCall(f: Expr, ps: Seq[Expr], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = (f +: ps).foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
  case class Member(e: Expr, member: String, t: InferredType, pos: Position) extends Value
  case class CompMem(e: Expr, prop: Expr, t: InferredType, pos: Position) extends Value
  case class New(e: Expr, ps: Seq[Expr], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = (e +: ps).foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
  case class CommaList(es: Seq[Expr], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = es.foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
  case class JsObject(e: Map[String, Expr], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = e.values.foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
  case class JsArray(e: Seq[Expr], t: InferredType, pos: Position) extends Value {
    override def typeAnnotations(m: Map[Int, TypeLines]) = e.foldLeft(addAnnotation(m, pos, t))((types, stmt) => stmt.typeAnnotations(types))
  }
}
