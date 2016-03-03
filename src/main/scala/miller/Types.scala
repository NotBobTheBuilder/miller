package miller

import miller.ASTf.{Ident, Expr}

import scala.sys.Prop

sealed trait JSType {
  def serialize(implicit st: ScopeStack) = this.toString
}

case object TNumber extends JSType {
  override def toString = "Number"
}
case object TString extends JSType {
  override def toString = "String"
}
case object TBoolean extends JSType {
  override def toString = "Boolean"
}
case object TUndefined extends JSType {
  override def toString = "Undefined"
}
case object TNull extends JSType {
  override def toString = "Null"
}

case class TFunction(params: Seq[IntersectT], result: InferredType) extends JSType {
  override def serialize(implicit st: ScopeStack) =
    "Function " + params.map({ case IntersectT(i) => s"{ $i : ${ st.getGroupType(i) } }" }).mkString("(", ", ", ")") + " -> " + result.serialize

  def specialise(st: ScopeStack): TFunction = {
    // TODO specialise function type
    // Should copy type signature replacing ids to new ones
    this.copy()
  }
}
case class TObject(properties: Map[String, InferredType]) extends JSType
case class TArray(t: InferredType) extends JSType

sealed trait InferredType {

  def serialize(implicit st: ScopeStack): String = this match {
    case AnyT           => "Any"
    case t: TypeError   => t.toString
    case ConstT(t)      => t.serialize
    case SetT(ts)       => ts.map(_.serialize).mkString("{ ", " v ", "}")
    case VarT(v)        => st.getType(v).toString
    case IntersectT(t)  => s"{ $t : ${st.getGroupType(t).serialize } }"
  }

  def intersect(other: InferredType)(implicit st: ScopeStack): InferredType = {
    // TODO Consider unification vs pattern matching here
    // Functional Progrmming Field & Harisson - Algorithm W / Type Checking etc
    this match {
      case AnyT           => other
      case t: TypeError   => this
      case ConstT(t)      => other match {
        case ConstT(u)      => if (t == u)                ConstT(t) else NoInterErr(Set(ConstT(t), ConstT(u)))
        case SetT(ts)       => if (ts contains t)         ConstT(t) else NoInterErr(Set(SetT(ts), ConstT(t)))
        case VarT(v)        =>
          val t2 = st.getType(v) intersect this
          st.setType(v, t2)
          t2
        case IntersectT(i)  => st.setGroupType(i, ConstT(t)); other
        case _              => other intersect this
      }
      case SetT(ts)       => other match {
        case SetT(us)       => if ((ts intersect us).nonEmpty) { SetT(ts intersect us) } else { NoInterErr(Set(SetT(ts), SetT(us)))}
        case VarT(v)        => st.getType(v) intersect this
        case IntersectT(i)  => st.setGroupType(i, SetT(ts)); other
        case _              => other intersect this
      }
      case VarT(v)        => other match {
        case VarT(w)        => IntersectT(st.link(Set(w, v)))
        case IntersectT(i)  => st.setVarType(v, i); IntersectT(i)
        case _              => other intersect this
      }
      case IntersectT(i)  => other match {
        case IntersectT(v)  => IntersectT(st.mergeGroupTypes(Set(i, v)))
        case _              => other intersect this
      }
    }
  }

  def isA(t: JSType)(implicit st: ScopeStack): Boolean = this match {
    case ConstT(u) => u == t
    case VarT(v) => st.getType(v) isA t
    case IntersectT(i) => st.getGroupType(i) isA t
    case _ => false
  }

  def canSatisfy(other: InferredType)(implicit st: ScopeStack): Boolean = other match {
    case AnyT => true
    case ConstT(t) => this isA t
    case VarT(id) => this canSatisfy st.getType(id)
    case IntersectT(id) => this canSatisfy st.getGroupType(id)
    case SetT(ss) => ss.exists(this canSatisfy ConstT(_))
    case t: TypeError => false
  }
}

case object AnyT extends InferredType

case class ConstT(t: JSType) extends InferredType {
  override def toString = t.toString
}
case class SetT(ts: Set[JSType]) extends InferredType {
  override def toString = ts.mkString("[", ", ", "]")
}
case class VarT(v: Int) extends InferredType
case class IntersectT(vs: Int) extends InferredType

sealed trait TypeError extends InferredType
case class NoInterErr(possibles: Set[InferredType]) extends TypeError
case class BadArgsErr(f: TFunction, e: Seq[InferredType]) extends TypeError

class ScopeStack {
  type VarID = Int
  type GroupID = Int

  private val vars = collection.mutable.HashMap[VarID, String]()
  private val scopes = collection.mutable.Stack(collection.mutable.HashMap[String, VarID]())
  private val signature = collection.mutable.Stack[(Seq[VarT], InferredType)]()

  val varGroups = collection.mutable.HashMap[VarID, GroupID]()
  val mergedGroups = collection.mutable.HashMap[GroupID, GroupID]()
  val groupTypes = collection.mutable.HashMap[GroupID, InferredType]()

  var _varUID = 0
  var _typeUID = 0

  def varUID(): Int = { _varUID += 1; _varUID }
  def typeUID(): Int = { _typeUID += 1; _typeUID }

  def getId(name: String): Option[Int] =
    scopes.collectFirst {
      /*  TODO: Refactor this
       * Should take in an expression and:
       * - if it's not a variable return a typeerror
       * - if it's a property, find the type of the property (presumably a property check and recursive call)
       * - if it's a variable, return the type
       */
    case x if x.contains(name) => x.get(name).get
  }

  def declare(n: String): Int = {
    val id = varUID()
    vars += (id -> n)
    scopes.last += (n -> id)
    define(id, AnyT)
    id
  }

  def declare(n: String, t: InferredType): Int = {
    val id = declare(n)
    define(id, t)
    id
  }

  def define(v: Int, t: InferredType): Unit = {
    val id = typeUID()
    varGroups += (v -> id)
    groupTypes += (id -> t)
  }

  def getName(id: Int): Option[String] = vars.get(id)

  def pushScope(params: Seq[String]): Seq[Int] = {
    val ps = params.map(_ -> varUID())

    scopes push collection.mutable.HashMap(ps.toSeq: _*)
    signature push (ps.map(p => VarT(p._2)) -> AnyT)

    ps.foreach(vars += _.swap)
    ps.foreach(p => define(p._2, AnyT))

    ps.map(_._2)
  }

  def popScope(): Unit = {
    scopes.pop()
    signature.pop()
  }

  def ret(t: InferredType): Unit = {
    implicit val st: ScopeStack = this

    signature.push(signature.pop() match {
      case (ps, r) => (ps, r intersect t)
    })
  }
  
  def returnType = signature.top._2 match {
    case AnyT => ConstT(TUndefined)
    case x    => x
  }

  def paramTypes(implicit st: ScopeStack): Seq[IntersectT] = signature.top._1.map {
    case VarT(i) =>
      val n = varGroups.get(i).get
      IntersectT(mergedGroups.getOrElse(n, n))
  }
  
  def link(varIds: Set[VarID]): GroupID = {
    implicit val st: ScopeStack = this

    val varsToGroups = varIds.flatMap(varGroups.get)
    val groupId = varsToGroups.min // We'll merge all the other groups into the one with the smallest ID

    val others = varsToGroups.filter(_ != groupId)

    val t = varIds
      .flatMap(varGroups.get(_).flatMap(groupTypes.remove).toSeq)
      .foldLeft(AnyT: InferredType)(_ intersect _)

    others.foreach(mergedGroups += _ -> groupId)
    groupTypes += (groupId -> t)
    groupId
  }

  def setType(node: Int, t: InferredType): Int = {
    val id = typeUID()
    varGroups += node -> id
    groupTypes += (id -> t)
    id
  }

  def getType(nodeID: Int): InferredType = {
    varGroups
      .get(nodeID)
      .map(a => mergedGroups.getOrElse(a, a))
      .flatMap(groupTypes.get).getOrElse(AnyT)
  }

  def setGroupType(vt: Int, t: InferredType): Unit = {
    implicit val st: ScopeStack = this
    groupTypes += (vt -> (groupTypes.getOrElse(vt, AnyT) intersect t))
  }

  def getGroupType(gid: Int): InferredType = {
    groupTypes.get(mergedGroups.getOrElse(gid, gid)).get
  }

  def mergeGroupTypes(vt: Set[Int]): Int = {
    implicit val st: ScopeStack = this

    vt.foreach(mergedGroups += _ -> vt.min)
    groupTypes += vt.min -> vt.flatMap(groupTypes.get).reduceRight(_ intersect _)
    vt.min
  }

  def setVarType(vt: Int, gt: Int): Unit = {
    varGroups += (vt -> gt)
  }

  def typeOf(v: String): Seq[InferredType] = {
    vars
      .filter(_._2 == v).keys
      .flatMap(varGroups.get)
      .flatMap(groupTypes.get)
      .toSeq
  }

  override def toString = {
    /* TODO: display all variables in all scopes
     */
    // (Int, InferredType) -> (Set(Int), InferredType) -> (Set(String), InferredType)
    groupTypes map {
      case (id, t) => varGroups.filter(_._2 == id).keySet.flatMap(vars.get) -> t
    } map {
      case (names, t) => names.mkString("[ ", ", ", " ]") + " : " + t
    } mkString("{\n", "\n", "\n}")
  }
}
