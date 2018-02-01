package lambda

case class Context(list: List[Type]) {
  def +(t: Type): Context = Context(t :: list)

  def apply(k: Int): Type = list(k)

  def length: Int = list.length

  override def toString: String = list.reverse.zip('a' to 'z').map({ case (x, t) => s"$x : $t" }).mkString(", ")
}

object Context {
  val empty: Context = Context(TFloat :: TFloat :: Nil)
}
