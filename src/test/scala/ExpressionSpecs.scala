import miller._
import org.scalacheck.{Arbitrary, Gen, Properties, Prop}
import Prop._
import Arbitrary.arbitrary

object ExpressionSpecs extends Properties("Expression") {
  import ASTf.Program.{expr2f, statement2f, program2f}
  import AST._
  val pos = Position(0, 0, 0, 0)

  case class IdentName(s: String)

  val emptyObject = JsObject(Map(), pos)

  implicit lazy val identName: Arbitrary[IdentName] = Arbitrary(Gen.identifier.map(IdentName))
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
    Gen.nonEmptyListOf(arbitrary[ConstT])
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

  implicit lazy val jsArr: Arbitrary[JsArray] = Arbitrary(Gen.oneOf(
    Gen.nonEmptyListOf(arbitrary[LiteralNum]).map(ns => JsArray(ns, pos)),
    Gen.nonEmptyListOf(arbitrary[LiteralStr]).map(ns => JsArray(ns, pos))
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

  new Properties("Type Rules") {
    property("Variable") =         forAll { (id: Ident, p: Expr) =>
      implicit val st: ScopeStack = new ScopeStack

      statement2f(Declare(Seq(id.name -> Some(p)), pos))(st)

      st.typeOf(id.name).headOption.contains(expr2f(p).t)
    }

    property("Call") = forAll { (name: Option[IdentName], ps: List[(Ident, Expr)], rt: (Ident, Expr)) =>
      implicit val i: ScopeStack = new ScopeStack

      expr2f(JsFunction(name.map(_.s), rt._1.name +: ps.map(_._1.name), Seq(Return(rt._1, pos)), pos))
        .t
        .applyCall(expr2f(rt._2) +: ps.map(_._2).map(expr2f))
        .actualT == rt._2.t
    }

    property("Define") = forAll { (i: IdentName, e: Expr) =>
      implicit val st: ScopeStack = new ScopeStack
      statement2f(Declare(Seq(i.s -> Some(e)), pos))
      val programTypes = st.typeOf(i.s)
      programTypes.length == 1 && programTypes.head == e.t
    }

    property("Declare") = forAll { (i: IdentName) =>
      implicit val st: ScopeStack = new ScopeStack
      statement2f(Declare(Seq(i.s -> None), pos))
      val programTypes = st.typeOf(i.s)
      programTypes.length == 1 && programTypes.head == AnyT
    }

    property("Property") = forAll(
      Gen.nonEmptyListOf(arbitrary[(IdentName, Expr)])
    ) { (props) =>
      implicit val st: ScopeStack = new ScopeStack

      val ps = props.map { case (id, exp) => id.s -> exp }
      val (propN, propT) = props.head
      Member(JsObject(ps.toMap, pos), propN.s, pos).t == propT.t
    }

    property("Array Element") = forAll { (a: JsArray) =>
      implicit val st: ScopeStack = new ScopeStack

      a.t match {
        case ConstT(TArray(t)) => t == CompMem(a, LiteralNum("0", "0", pos), pos).t
        case _ => false
      }
    }

    property("Computed Member") = forAll (
      Gen.const(emptyObject),
      arbitrary[IdentName]
    ) { (obj, prop) =>
      implicit val st: ScopeStack = new ScopeStack
      CompMem(obj, LiteralStr(prop.s, pos), pos).t == AnyT
    }

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

    property("Object Intersection") = forAll { (commonProp: (IdentName, DirectType),
                                                ps1: Seq[(IdentName, DirectType)],
                                                ps2: Seq[(IdentName, DirectType)]) =>
      implicit val st: ScopeStack = new ScopeStack

      val o1 = TObject(collection.mutable.Map((commonProp._1.s -> commonProp._2) +: ps1.map(i => i._1.s -> i._2): _*))
      val o2 = TObject(collection.mutable.Map((commonProp._1.s -> commonProp._2) +: ps2.map(i => i._1.s -> i._2): _*))

      o1 intersect o2 match {
        case ConstT(TObject(ps)) => ps.nonEmpty && ps(commonProp._1.s) == commonProp._2
        case _ => false
      }

    }

    property("Array Intersection") = forAll { (a1: TArray, a2: TArray) =>
      implicit val st: ScopeStack = new ScopeStack

      a1.t intersect a2.t match {
        case e: TypeError => (ConstT(a1) intersect ConstT(a2)).isInstanceOf[TypeError]
        case exp => (ConstT(a1) intersect ConstT(a2)) == ConstT(TArray(exp))
      }
    }

    property("Function Intersection") = forAll { (f1: TFunction, f2: TFunction) =>
      implicit val st: ScopeStack = new ScopeStack

      f1 intersect f2 match {
        case e: TypeError => (ConstT(f1) intersect ConstT(f2)).isInstanceOf[TypeError]
        case exp => (ConstT(f1) intersect ConstT(f2)) == exp
      }
    }

    property("Any intersection") = forAll { (t: DirectType) =>
      implicit val st: ScopeStack = new ScopeStack
      (AnyT intersect t) == t
    }

    property("SetT intersection") = forAll { (c: ConstT, s: SetT) =>
      implicit val st: ScopeStack = new ScopeStack
      (s.copy(ts = s.ts + c.t) intersect c) == c
    }
  }.main(Array())

}
