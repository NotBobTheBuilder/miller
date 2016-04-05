package miller

import miller.ASTf.Expr

sealed trait JsType {
  def serialize(implicit st: ScopeStack) = this.toString

  val properties: collection.Map[String, InferredType] = Map[String, InferredType]()
}

case object TNumber extends JsType {
  override def toString = "Number"
}
case object TRegExp extends JsType {
  override def toString = "Number"
}
case object TString extends JsType {
  override def toString = "String"
}
case object TBoolean extends JsType {
  override def toString = "Boolean"
}
case object TUndefined extends JsType {
  override def toString = "Undefined"
}
case object TNull extends JsType {
  override def toString = "Null"
}

case class TFunction(params: Seq[InferredType], result: InferredType) extends JsType {
  override def serialize(implicit st: ScopeStack) =
    "Function " + params.map(_.serialize).mkString("(", ", ", ")") + " -> " + result.serialize

  def specialise(implicit st: ScopeStack) = TFunction.tupled(st.copyFuncType(this.params, this.result))

  def callwith(ps: Seq[ASTf.Expr])(implicit st: ScopeStack): InferredType = {
    if (this.params.length != ps.length) {
      BadArgsErr(this, ps.map(_.t))
    } else {
      val TFunction(params, ret) = this.specialise

      params.zip(ps).foldLeft(ret) { case (r, (expectedType, calledExpr)) =>
        expectedType intersect calledExpr.t match {
          case e: NoInterErr => e
          case _ => r
        }
      }
    }
  }
}

case class TObject(override val properties: collection.mutable.Map[String, InferredType]) extends JsType {

  override def serialize(implicit st: ScopeStack) = properties.map({ case (k, v) => k + ": " + v.serialize }).mkString("Object { ", ", ", " }")

  def intersect(that: TObject)(implicit st: ScopeStack) = {
    val props = this.properties
      .keys
      .filter(that.properties.contains)
      .map(k => k -> (this.properties.get(k).get intersect that.properties.get(k).get))
      .toSeq

    if (props.isEmpty) {
      NoInterErr(Set(ConstT(this), ConstT(that)))
    } else {
      ConstT(TObject(collection.mutable.Map(props: _*)))
    }
  }
}

case class TArray(t: InferredType) extends JsType {
  override val properties = Map("length" -> ConstT(TNumber))

  override def serialize(implicit st: ScopeStack) = "Array<" + t.serialize + ">"
}

sealed trait InferredType {

  def serialize(implicit st: ScopeStack): String = this match {
    case AnyT           => "Any"
    case t: TypeError   => t.toString
    case ConstT(t)      => t.serialize
    case SetT(ts)       => ts.map(_.serialize).mkString("{ ", " v ", " }")
    case IntersectT(t)  => s"{ $t : ${ st.getGroupType(GroupID(t)).serialize } }"
  }

  def intersect(other: InferredType)(implicit st: ScopeStack): InferredType = {
    // TODO Consider unification vs pattern matching here
    // Functional Programming Field & Harisson - Algorithm W / Type Checking etc
    this match {
      case AnyT           => other
      case t: TypeError   => this
      case ConstT(t)      => other match {
        case ConstT(u)    => (t, u) match {
                               case (t1: TObject, t2: TObject) => t1 intersect t2
                               case _ if t == u => ConstT(t)
                               case _ => NoInterErr(Set(ConstT(t), ConstT(u)))
                             }

        case SetT(ts)       =>  if (ts contains t)         ConstT(t) else NoInterErr(Set(SetT(ts), ConstT(t)))
        case IntersectT(i)  =>  if ((this canSatisfy other) || (other canSatisfy this)) {
                                  st.filterGroupType(GroupID(i), ConstT(t))
                                } else {
                                  NoInterErr(Set(ConstT(t), st.getGroupType(GroupID(i))))
                                }
        case _              => other intersect this
      }
      case SetT(ts)       => other match {
        case SetT(us)       => if ((ts intersect us).nonEmpty) { SetT(ts intersect us) } else { NoInterErr(Set(SetT(ts), SetT(us)))}
        case IntersectT(i)  => st.filterGroupType(GroupID(i), SetT(ts)); other
        case _              => other intersect this
      }
      case IntersectT(i)  => other match {
        case IntersectT(v)  => IntersectT(st.mergeGroupTypes(Set(GroupID(i), GroupID(v))).id)
        case _              => other intersect this
      }
    }
  }

  def isA(t: JsType)(implicit st: ScopeStack): Boolean = this match {
    case ConstT(TFunction(ps, r)) => t.isInstanceOf[TFunction]
    case ConstT(u) => u == t
    case IntersectT(i) => this.actualT isA t
    case _ => false
  }

  def canSatisfy(other: InferredType)(implicit st: ScopeStack): Boolean = other match {
    case AnyT => true
    case ConstT(t) => this isA t
    case IntersectT(id) => this canSatisfy st.getGroupType(GroupID(id))
    case SetT(ss) => ss.exists(this canSatisfy ConstT(_))
    case t: TypeError => false
  }

  def actualT(implicit st: ScopeStack): DirectType = this match {
    case IntersectT(i) => st.getGroupType(GroupID(i)).actualT
    case a: DirectType => a
  }

  def applyCall(ps: Seq[Expr])(implicit st: ScopeStack): InferredType = {
    this match {
      case ConstT(t: TFunction) => t.callwith(ps)
      case IntersectT(i) => st.getGroupType(GroupID(i)).applyCall(ps)
      case other => other
    }
  }
}

// Basically everything that's not an IntersectT
sealed trait DirectType extends InferredType

case object AnyT extends InferredType with DirectType

case class ConstT(t: JsType) extends InferredType with DirectType {
  override def toString = t.toString
}

case class SetT(ts: Set[JsType]) extends InferredType with DirectType {
  override def toString = ts.mkString("[", ", ", "]")
}

case class IntersectT(vs: Int) extends InferredType

sealed trait TypeError extends InferredType with DirectType

case class NoInterErr(possibles: Set[InferredType]) extends TypeError
case class BadArgsErr(f: TFunction, e: Seq[InferredType]) extends TypeError
case class NotAProperty(obj: TObject, property: String) extends TypeError
case class NotAssignableErr(assignee: InferredType, value: InferredType) extends TypeError


case class VarID(id: Int)
case class GroupID(id: Int) extends Ordered[GroupID] {
  def compare(other: GroupID) = this.id compare other.id
}

class ScopeStack {
  private val vars = collection.mutable.HashMap[VarID, String]()
  private val scopes = collection.mutable.Stack(collection.mutable.HashMap[String, VarID]())
  private val signature = collection.mutable.Stack[(Seq[GroupID], InferredType)]()

  val varGroups = collection.mutable.HashMap[VarID, GroupID]()
  val mergedGroups = collection.mutable.HashMap[GroupID, GroupID]()
  val groupTypes = collection.mutable.HashMap[GroupID, InferredType]()

  var _varUID = 0
  var _typeUID = 0

  private def varUID(): VarID =     { _varUID += 1; VarID(_varUID) }
  private def groupUID(): GroupID = { _typeUID += 1; GroupID(_typeUID) }

  definePervasives()

  def getId(name: String): Option[VarID] = {
    scopes.collectFirst {
      /*  TODO: Refactor this
         * Should take in an expression and:
         * - if it's not a variable return a typeerror
         * - if it's a property, find the type of the property (presumably a property check and recursive call)
         * - if it's a variable, return the type
         */
      case x if x.contains(name) => x.get(name).get
    }
  }

  def declare(n: String, t: InferredType = AnyT): (VarID, GroupID) = {
    val vid = varUID()
    val gid = groupUID()

    vars += (vid -> n)
    scopes.top += (n -> vid)

    varGroups += (vid -> gid)
    groupTypes += (gid -> t)

    vid -> gid
  }

  def getName(id: VarID): Option[String] = vars.get(id)

  def pushScope(params: Seq[String]): Seq[VarID] = {
    val ps = params.map { ident =>
      val (vid, gid) = declare(ident)
      (ident, vid, gid)
    }

    scopes push collection.mutable.HashMap(ps.map(v => v._1 -> v._2): _*)
    signature push (ps.map(_._3) -> AnyT)

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

  def paramTypes(implicit st: ScopeStack): Seq[IntersectT] =
    signature.top._1.map { group =>
      IntersectT(mergedGroups.getOrElse(group, group).id)
    }

  def getType(nodeID: VarID): InferredType = {
    varGroups
      .get(nodeID)
      .map(a => mergedGroups.getOrElse(a, a))
      .flatMap(groupTypes.get).getOrElse(AnyT)
  }

  def filterGroupType(vt: GroupID, t: InferredType): InferredType = {
    implicit val st: ScopeStack = this
    val refinedT = groupTypes.getOrElse(vt, AnyT) intersect t
    groupTypes += (vt -> refinedT)
    refinedT
  }

  def getGroupType(gid: GroupID): InferredType = {
    groupTypes.get(mergedGroups.getOrElse(gid, gid)).get
  }

  def getGroup(v: VarID): GroupID = {
    varGroups.get(v).map(v => mergedGroups.getOrElse(v, v)).get
  }

  def mergeGroupTypes(varIds: Set[GroupID]): GroupID = {
    implicit val st: ScopeStack = this
    val groupId = varIds.min // We'll merge all the other groups into the one with the smallest ID
    val others = varIds - groupId

    val t = varIds.flatMap(groupTypes.remove).reduceRight(_ intersect _)

    others.foreach(mergedGroups += _ -> groupId)
    groupTypes += (groupId -> t)
    groupId
  }

  def copyGroupType(gid: GroupID): GroupID = {
    val newId = groupUID()

    groupTypes += (newId -> groupTypes.get(mergedGroups.getOrElse(gid, gid)).get)

    newId
  }

  def copyFuncType(vars: Seq[InferredType], ret: InferredType): (Seq[InferredType], InferredType) = {
    val gidCache = collection.mutable.HashMap[GroupID, GroupID]()
    val copyGid = { (i: InferredType) => i match {
        case (gid: IntersectT) => IntersectT(gidCache.getOrElseUpdate(GroupID(gid.vs), copyGroupType(GroupID(gid.vs))).id)
        case other => other
      }
    }

    def returnCopy(r: InferredType): InferredType = r match {
      case t: IntersectT => copyGid(t)
      case ConstT(f: TFunction) => ConstT(TFunction.tupled(f.params.map(copyGid), returnCopy(f.result)))
      case anyOther => anyOther
    }

    (vars.map(copyGid), returnCopy(ret))
  }

  def typeOf(v: String): Seq[InferredType] = {
    vars
      .filter(_._2 == v).keys
      .flatMap(varGroups.get)
      .map(g => mergedGroups.getOrElse(g, g))
      .flatMap(groupTypes.get)
      .toSeq
  }

  override def toString = {
    /* TODO: mergedGroups
     */
    // (GroupID, InferredType) -> (Set(GroupID), InferredType) -> (Set(VarID), InferredType) -> (Set(String), InferredType)
    groupTypes map {
        case (id, t) => (mergedGroups.toSet.filter(_._2 == id).map(_._1) + id) -> t
    } map {
        case (gids, t) => varGroups.filter(gid => gids.contains(gid._2)).keySet.flatMap(vars.get) -> t
    } map {
      case (names, t) => names.mkString("[ ", ", ", " ]") + " : " + t.serialize(this)
    } mkString("{\n", "\n", "\n}")
  }

  def definePervasives() = {
    this.declare("console", ConstT(TObject(collection.mutable.Map(
      "log" -> ConstT(TFunction(Seq(AnyT), ConstT(TUndefined)))
    ))))
    this.declare("alert", ConstT(TFunction(Seq(AnyT), ConstT(TUndefined))))
  }
}
