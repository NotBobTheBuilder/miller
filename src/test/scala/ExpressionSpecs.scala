import miller._
import org.scalacheck.{Arbitrary, Gen, Properties, Prop}
import Prop._
import Arbitrary.arbitrary

object ExpressionSpecs extends Properties("Expression") {
  import ASTf.Program.{expr2f, statement2f, program2f}
  import AST._
  val pos = Position(0, 0, 0, 0)

  case class IdentName(s: Option[String])

  implicit lazy val identName: Arbitrary[IdentName] = Arbitrary(Gen.option(Gen.identifier).map(IdentName))
  implicit lazy val ident: Arbitrary[Ident] = Arbitrary(Gen.identifier.map(s => Ident(s, pos)))
  implicit lazy val num: Arbitrary[LiteralNum] = Arbitrary(Gen.posNum[Int].map(n => LiteralNum(n.toString, "0", pos)))
  implicit lazy val str: Arbitrary[LiteralStr] = Arbitrary(Gen.alphaStr.map(s => LiteralStr(s, pos)))
  implicit lazy val ret: Arbitrary[Return] = Arbitrary(num.arbitrary.map(n => Return(n, pos)))

  implicit lazy val func: Arbitrary[TFunction] = Arbitrary(for {
    params <- arbitrary[Seq[DirectType]]
    ret <- arbitrary[DirectType]
  } yield TFunction(params, ret))

  implicit lazy val arr: Arbitrary[TArray] = Arbitrary(for {
    t <- arbitrary[DirectType]
  } yield TArray(t))

  implicit lazy val obj: Arbitrary[TObject] = Arbitrary(for {
    size <- Gen.posNum[Int]
    keys <- Gen.listOfN(size, Gen.identifier)
    vals <- Gen.listOfN(size, arbitrary[DirectType])
  } yield TObject(collection.mutable.Map(keys.zip(vals): _*)))

  implicit lazy val genFunction: Arbitrary[JsFunction] = Arbitrary(for {
    name <- Gen.option(Gen.identifier)
    params <- Gen.listOf(Gen.identifier)
    block <- Gen.option(arbitrary[Return])
  } yield JsFunction(name, params, block.toSeq, pos))

  implicit lazy val ct: Arbitrary[ConstT] = Arbitrary(Gen.oneOf(
    Gen.const(TNumber),
    Gen.const(TRegExp),
    Gen.const(TString),
    Gen.const(TBoolean),
    Gen.const(TUndefined),
    Gen.const(TNull) //,
//    arbitrary[TFunction],
//    arbitrary[TArray],
//    arbitrary[TObject]
  ).map(a => ConstT(a)))

  implicit lazy val st: Arbitrary[SetT] = Arbitrary(
    Gen.listOf(arbitrary[ConstT])
      .map(_.toSet.map((p: ConstT) => p.t))
      .map(SetT)
  )

  implicit lazy val dt: Arbitrary[DirectType] = Arbitrary(Gen.oneOf(
    arbitrary[ConstT],
    arbitrary[SetT],
    Gen.const(AnyT)
  ))

  implicit lazy val genStatement: Arbitrary[Statement] = Arbitrary(Gen.oneOf(
    num.arbitrary,
    str.arbitrary
  ))

  implicit lazy val expr: Arbitrary[Expr] = Arbitrary(Gen.oneOf(
    num.arbitrary,
    str.arbitrary
  ))

  val addChain: (Expr, Seq[Expr]) => Expr = (h, t) => t.foldLeft(h)(Add(_, _, pos))

  {
    implicit val emptyScopeStack: ScopeStack = new ScopeStack

    property("parsing ints") = forAll { num: Int =>
      def matches(e: ASTf.Expr): Boolean = e match {
        case e: ASTf.LiteralNum => e.t == ConstT(TNumber)
        case u: ASTf.USub => matches(u.exp)
        case _ => false
      }

      matches(Parsing.parseExpr(num.toString))
    }

    property("int literals") = forAll { num: LiteralNum => num.t isA TNumber }
    property("int add") = forAll { (h: LiteralNum, t: Seq[LiteralNum]) => addChain(h, t).t isA TNumber }
    property("string add") = forAll { (h: LiteralStr, t: Seq[LiteralStr]) => addChain(h, t).t isA TString }
  }

  new Properties("HM") {
    property("T-Var") =         forAll { (id: Ident, p: Expr) =>
      implicit val st: ScopeStack = new ScopeStack

      statement2f(Declare(Seq(id.name -> Some(p)), pos))(st)

      st.typeOf(id.name).headOption.contains(expr2f(p).t)
    }

    property("T-App") = forAll { (name: IdentName, ps: List[(Ident, Expr)], rt: (Ident, Expr)) =>
      implicit val i: ScopeStack = new ScopeStack

      expr2f(JsFunction(name.s, rt._1.name +: ps.map(_._1.name), Seq(Return(rt._1, pos)), pos))
        .t
        .applyCall(expr2f(rt._2) +: ps.map(_._2).map(expr2f))
        .actualT == rt._2.t
    }

    property("T-Lam") =   forAll { (id: Ident, ret: Expr, call: Expr) => true }

    property("T-Let") =   forAll { (i: Int) => true }

    property("T-Gen") =   forAll { (i: Int) => true }

    property("T-Inst") =  forAll { (i: Int) => true }

  }.main(Array())

  new Properties("Unification") {
    property("Const intersection") =     forAll { (lhs: ConstT, rhs: ConstT) =>
      implicit val st: ScopeStack = new ScopeStack

      lhs intersect rhs match {
        case ConstT(t)      if t == lhs.t && t == rhs.t => true
        case NoInterErr(ts) if lhs.t != rhs.t && ts == Set(lhs, rhs) => true
        case _ => false
      }
    }
  }.main(Array())

}
