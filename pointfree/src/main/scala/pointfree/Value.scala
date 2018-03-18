package pointfree

abstract class Value {
  def unwrap: Any
}

object Value {
  def wrap(v: Any): Value = v match {
    case v: Int => VInt(v)
    case v: Float => VFloat(v)
    case v: Boolean => VBool(v)
    case v: List[Any] => VList(v.map(e => wrap(e)))
    case v: (Any, Any) => VPair(wrap(v._1), wrap(v._2))
//    case v: (Value => Value) => VFun(v)
    case _ => VUndefined
  }
}

case class VInt(v: Int) extends Value {
  override def unwrap: Any = v
}

case class VFloat(v: Float) extends Value {
  override def unwrap: Any = v
}

case class VBool(v: Boolean) extends Value {
  override def unwrap: Any = v
}

case class VList(v: List[Value]) extends Value {
  override def unwrap: Any = v.map(e => e.unwrap)
}

case class VPair(fst: Value, snd: Value) extends Value {
  override def unwrap: Any = (fst.unwrap, snd.unwrap)
}

case class VFun(v: Value => Value) extends Value {
  override def unwrap: Any = ???
}

case object VUndefined extends Value {
  override def unwrap: Any = ???
}