package miller

sealed trait JSType {
  def serialize(implicit st: ScopeStack) = this.toString
}

case object TNumber extends JSType {
  override def toString = "Number"
}
case object TRegExp extends JSType {
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
    "Function " + params.map({ case IntersectT(i) => s"{ $i : ${ st.getGroupType(GroupID(i)) } }" }).mkString("(", ", ", ")") + " -> " + result.serialize

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
    case IntersectT(t)  => s"{ $t : ${st.getGroupType(GroupID(t)).serialize } }"
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
        case IntersectT(v)  =>
//          println(s"$i (${st.getGroupType(GroupID(i))}) intersect $v (${st.getGroupType(GroupID(v))})")
          IntersectT(st.mergeGroupTypes(Set(GroupID(i), GroupID(v))).id)
        case _              => other intersect this
      }
    }
  }

  def isA(t: JSType)(implicit st: ScopeStack): Boolean = this match {
    case ConstT(TFunction(ps, r)) => t.isInstanceOf[TFunction]
    case ConstT(u) => u == t
    case IntersectT(i) => st.getGroupType(GroupID(i)) isA t
    case _ => false
  }

  def canSatisfy(other: InferredType)(implicit st: ScopeStack): Boolean = other match {
    case AnyT => true
    case ConstT(t) => this isA t
    case IntersectT(id) => this canSatisfy st.getGroupType(GroupID(id))
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
case class IntersectT(vs: Int) extends InferredType

sealed trait TypeError extends InferredType
case class NoInterErr(possibles: Set[InferredType]) extends TypeError
case class BadArgsErr(f: TFunction, e: Seq[InferredType]) extends TypeError


case class VarID(id: Int)
case class GroupID(id: Int) extends Ordered[GroupID] {
  def compare(other: GroupID) = this.id compare other.id
}

class ScopeStack {
  private val vars = collection.mutable.HashMap[VarID, String]()
  private val scopes = collection.mutable.Stack(collection.mutable.HashMap[String, VarID]())
  private val signature = collection.mutable.Stack[(Seq[GroupID], InferredType)]()

  private val varGroups = collection.mutable.HashMap[VarID, GroupID]()
  private val mergedGroups = collection.mutable.HashMap[GroupID, GroupID]()
  private val groupTypes = collection.mutable.HashMap[GroupID, InferredType]()

  var _varUID = 0
  var _typeUID = 0

  private def varUID(): VarID =     { _varUID += 1; VarID(_varUID) }
  private def groupUID(): GroupID = { _typeUID += 1; GroupID(_typeUID) }

  def getId(name: String): Option[VarID] =
    scopes.collectFirst {
      /*  TODO: Refactor this
       * Should take in an expression and:
       * - if it's not a variable return a typeerror
       * - if it's a property, find the type of the property (presumably a property check and recursive call)
       * - if it's a variable, return the type
       */
    case x if x.contains(name) => x.get(name).get
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
    // TODO ???
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
    /* TODO: This should maintain a list of things attempted to be returned and ensure they're consistent
     * TODO: Diverging returns like if(a) { return 1; } else { return "a"; } should be checked & rejected
     */
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

//  def mergeGroupTypes(vt: Set[GroupID]): GroupID = {
//    implicit val st: ScopeStack = this
//
//    vt.foreach(mergedGroups += _ -> vt.min)
//    groupTypes += vt.min -> vt.flatMap(groupTypes.get).reduceRight(_ intersect _)
//    vt.min
//  }

  def mergeGroupTypes(varIds: Set[GroupID]): GroupID = {
    implicit val st: ScopeStack = this
    val groupId = varIds.min // We'll merge all the other groups into the one with the smallest ID

    val others = varIds.filter(_ != groupId)

    val t = varIds
      .flatMap(groupTypes.remove).toSeq
      .reduceRight(_ intersect _)

    others.foreach(mergedGroups += _ -> groupId)
    groupTypes += (groupId -> t)
    groupId
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
