package miller

sealed trait JSType

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
case class TFunction(params: Seq[InferredType], result: InferredType) extends JSType {
  override def toString = "Function " + params.mkString("(", ", ", ")") + " -> " + result
}
case class TObject(properties: Map[String, InferredType]) extends JSType
case class TArray(t: InferredType) extends JSType

sealed trait InferredType {
  def intersect(other: InferredType)(implicit st: ScopeStack): InferredType = {
    this match {
      case AnyT             => other
      case TypeError(js)    => other match {
        case AnyT           => other intersect this
        case TypeError(ks)  => TypeError(js ++ ks)
        case ConstT(t)      => TypeError(js + other)
        case SetT(ts)       => TypeError(js + other)
        case VarT(v)        => TypeError(js + other)
        case CallT(v)       => TypeError(js + other)
        case IntersectT(t)  => TypeError(js + other)
      }
      case ConstT(t)      => other match {
        case AnyT           => other intersect this
        case TypeError(js)  => other intersect this
        case ConstT(u)      => if (t == u)         { ConstT(t) } else { TypeError(Set(ConstT(t), ConstT(u))) }
        case SetT(ts)       => if (ts contains t)  { ConstT(t) } else { TypeError(Set(SetT(ts), ConstT(t))) }
        case VarT(v)        => st.define(v, ConstT(t)); ConstT(t)
        case CallT(v)       => ConstT(t) // TODO: Check type constraint for return type of v
        case IntersectT(i)  => st.setType(i, ConstT(t)); other
      }
      case SetT(ts)       => other match {
        case AnyT           => other intersect this
        case TypeError(js)  => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(us)       => if ((ts intersect us).nonEmpty) { SetT(ts intersect us) } else { TypeError(Set(SetT(ts), SetT(us)))}
        case VarT(v)        => st.define(v, SetT(ts)); SetT(ts)
        case CallT(v)       => SetT(ts) // TODO: Check type constraint for return type of v
        case IntersectT(i)  => st.setType(i, SetT(ts)); other
      }
      case VarT(v)        => other match {
        case AnyT           => other intersect this
        case TypeError(ks)  => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => st.link(Set(w, v)); IntersectT(Set(v, w))
        case CallT(w)       => IntersectT(Set(v, w))
        case IntersectT(i)  => st.link(i + v); IntersectT(i + v)
      }
      case CallT(v)       => other match {
        case AnyT           => other intersect this
        case TypeError(ks)  => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => other intersect this
        case CallT(w)       => IntersectT(Set(w, v)) // TODO: Figure out this thing because it's probably (definitely) wrong
        case IntersectT(i)  => IntersectT(i + v)
      }
      case IntersectT(i)  => other match {
        case AnyT           => other intersect this
        case TypeError(ks)  => other intersect this
        case ConstT(t)      => other intersect this
        case SetT(ts)       => other intersect this
        case VarT(w)        => other intersect this
        case CallT(c)       => other intersect this
        case IntersectT(v)  => st.link(i ++ v); IntersectT(i ++ v)
      }
    }
  }

  def isA(t: JSType): Boolean = {
    this match {
      case ConstT(u) => u == t
      case _ => false
    }
  }
}

case object AnyT extends InferredType

case class ConstT(t: JSType) extends InferredType {
  override def toString = t.toString
}
case class SetT(ts: Set[JSType]) extends InferredType {
  override def toString = ts.mkString("[", ", ", "]")
}
case class TypeError(possibles: Set[InferredType]) extends InferredType
case class VarT(v: Int) extends InferredType
case class CallT(f: Int) extends InferredType
case class IntersectT(vs: Set[Int]) extends InferredType

class ScopeStack {
  val vars = collection.mutable.HashMap[Int, String]()
  val scopes = collection.mutable.Stack(collection.mutable.HashMap[String, Int]())
  val signature = collection.mutable.Stack[(Seq[VarT], InferredType)]()
  val varGroups = collection.mutable.HashMap[Int, Int]()
  val groupTypes = collection.mutable.HashMap[Int, InferredType]()

  var _varUID = 0
  def varUID(): Int = {
    _varUID += 1
    _varUID
  }

  var _typeUID = 0
  def typeUID(): Int = {
    _typeUID += 1
    _typeUID
  }

  def getId(name: String): Option[Int] = scopes.collectFirst {
    case x if x.contains(name) => x.get(name).get
  }

  def declare(n: String): Int = {
    val id = varUID()
    vars += (id -> n)
    scopes.last += (n -> id)
    id
  }

  def declare(n: String, t: InferredType): Int = {
    val id = declare(n)
    define(id, t)
    id
  }

  def getName(id: Int): Option[String] = vars.get(id)

  def pushScope(params: Seq[String]): Unit = {
    val ps = params.map(_ -> varUID())

    scopes push collection.mutable.HashMap(ps.toSeq: _*)
    signature push (ps.map(p => VarT(p._2)) -> AnyT)

    ps.foreach(vars += _.swap)
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

  def paramTypes(implicit st: ScopeStack) = signature.top._1.map {
    case VarT(i) => st.getType(i) // TODO: This should indicate varGroup as well as type (for stuff that needs to be the same)
  }
  
  def link(nodes: Set[Int]): Unit = {
    implicit val st: ScopeStack = this

    setType(nodes, nodes
      .flatMap(varGroups.get(_).flatMap(groupTypes.remove).toSeq)
      .foldLeft(AnyT: InferredType)(_ intersect _)
    )
  }

  def define(v: Int, t: InferredType, id: Int = typeUID()): Unit = {
    implicit val st: ScopeStack = this

    varGroups += (v -> id)
    groupTypes += (id -> t)
  }

  def setType(nodes: Set[Int], t: InferredType): Unit = {
    val id = typeUID()
    nodes.foreach(varGroups += _ -> id)
    groupTypes += (id -> t)

    groupTypes.keySet.diff(varGroups.values.toSet).foreach(groupTypes.remove)
  }

  def getType(nodeID: Int): InferredType = {
    varGroups.get(nodeID).flatMap(groupTypes.get).getOrElse(AnyT)
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
