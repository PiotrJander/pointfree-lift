package pointfree

abstract class Value

case class VInt(v: Int) extends Value

case class VFloat(v: Float) extends Value

case class VBool(v: Boolean) extends Value

case class VList(v: List[Value]) extends Value

case class VPair(fst: Value, snd: Value) extends Value

case class VFun(v: Value => Value) extends Value

case object VUndefined extends Value