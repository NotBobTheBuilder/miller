package miller

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
    "Function " + params.map({ case IntersectT(i) => s"{ $i : ${st.groupTypes.get(i)} }" }).mkString("(", ", ", ")") + " -> " + result.serialize
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
    case IntersectT(t)  => s"{ $t : ${st.groupTypes.get(t).map(_.serialize).getOrElse(t.toString())} }"
  }

  def intersect(other: InferredType)(implicit st: ScopeStack): InferredType = {
    this match {
      case AnyT             => other
      case t: TypeError   => this
      case ConstT(t)      => other match {
        case AnyT           => other intersect this
        case ConstT(u)      => if (t == u)         { ConstT(t) } else { NoInterErr(Set(ConstT(t), ConstT(u))) }
        case SetT(ts)       => if (ts contains t)  { ConstT(t) } else { NoInterErr(Set(SetT(ts), ConstT(t))) }
        case VarT(v)        => if (st.getType(v) == this) { this } else { NoInterErr(Set(this, st.getType(v))) }
        case CallT(v)       => ConstT(t) // TODO: Check type constraint for return type of v
        case IntersectT(i)  => st.setGroupType(i, ConstT(t)); other
        case t: TypeError => other intersect this
      }
      case SetT(ts)       => other match {
        case AnyT           => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(us)       => if ((ts intersect us).nonEmpty) { SetT(ts intersect us) } else { NoInterErr(Set(SetT(ts), SetT(us)))}
        case VarT(v)        => st.define(v, SetT(ts)); SetT(ts)
        case CallT(v)       => SetT(ts) // TODO: Check type constraint for return type of v
        case IntersectT(i)  => st.setGroupType(i, SetT(ts)); other
        case t: TypeError => other intersect this
      }
      case VarT(v)        => other match {
        case AnyT           => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => IntersectT(st.link(Set(w, v)))
        case CallT(w)       => ???
        case IntersectT(i)  => st.setVarType(v, i); IntersectT(i)
        case t: TypeError => other intersect this
      }
      case CallT(v)       => other match {
        case AnyT           => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => other intersect this
        case CallT(w)       => ??? // TODO: Figure out this thing because it's probably (definitely) wrong
        case IntersectT(i)  => IntersectT(i + v)
        case t: TypeError => other intersect this
      }
      case IntersectT(i)  => other match {
        case AnyT           => other intersect this
        case NoInterErr(ks) => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => other intersect this
        case CallT(c)       => other intersect this
        case IntersectT(v)  => IntersectT(st.mergeGroupTypes(Set(i, v)))
      }
    }
  }

  def isA(t: JSType): Boolean = this match {
    case ConstT(u) => u == t
    case _ => false
  }

  def canSatisfy(other: InferredType)(implicit st: ScopeStack): Boolean = other match {
    case AnyT => true
    case ConstT(t) => this isA t
    case VarT(id) => this canSatisfy st.getType(id)
    case IntersectT(id) => this canSatisfy st.getGroupType(id)
    case SetT(ss) => ss.exists(this canSatisfy ConstT(_))
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
case class CallT(f: Int) extends InferredType
case class IntersectT(vs: Int) extends InferredType

sealed trait TypeError extends InferredType
case class NoInterErr(possibles: Set[InferredType]) extends TypeError

case class BadArgsErr(f: TFunction, e: Seq[InferredType]) extends TypeError

class ScopeStack {
  type VarID = Int
  type GroupID = Int

  val vars = collection.mutable.HashMap[Int, String]()
  val scopes = collection.mutable.Stack(collection.mutable.HashMap[String, Int]())
  val signature = collection.mutable.Stack[(Seq[VarT], InferredType)]()
  val varGroups = collection.mutable.HashMap[VarID, GroupID]()
  val mergedGroups = collection.mutable.HashMap[GroupID, GroupID]()
  val groupTypes = collection.mutable.HashMap[GroupID, InferredType]()

  var _varUID = 0
  var _typeUID = 0

  def varUID(): Int = { _varUID += 1; _varUID }
  def typeUID(): Int = { _typeUID += 1; _typeUID }

  def getId(name: String): Option[Int] = scopes.collectFirst {
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

  def define(v: Int, t: InferredType, id: Int = typeUID()): Unit = {
    implicit val st: ScopeStack = this

    varGroups += (v -> id)
    groupTypes += (id -> t)
  }

  def getName(id: Int): Option[String] = vars.get(id)

  def pushScope(params: Seq[String]): Seq[Int] = {
    val ps = params.map(_ -> varUID())

    scopes push collection.mutable.HashMap(ps.toSeq: _*)
    signature push (ps.map(p => VarT(p._2)) -> AnyT)

    ps.foreach(vars += _.swap)

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
    case VarT(i) => IntersectT(varGroups.get(i).get)
  }
  
  def link(nodes: Set[Int]): Int = {
    implicit val st: ScopeStack = this

    setType(nodes, nodes
      .flatMap(varGroups.get(_).flatMap(groupTypes.remove).toSeq)
      .foldLeft(AnyT: InferredType)(_ intersect _)
    )
    nodes.head
  }

  def setType(nodes: Set[Int], t: InferredType): Int = {
    val id = typeUID()
    nodes.foreach(varGroups += _ -> id)
    groupTypes += (id -> t)

    varGroups.values.toSet.diff(groupTypes.keySet).foreach(groupTypes.remove)
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
    groupTypes.map {
      case (id, t) => varGroups.filter(_._2 == id).keySet.flatMap(vars.get) -> t
    } map {
      case (names, t) => names.mkString("[ ", ", ", " ]") + " : " + t
    } mkString("{\n", "\n", "\n}")
  }
}
