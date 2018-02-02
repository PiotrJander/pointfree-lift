package lambda

object Context {
  type Context = List[Type]

  implicit class ContextOps(val context: Context) {
    def toStringContext: String = context.reverse.zip('u' to 'z').map({ case (t, x) => s"$x : $t" }).mkString(", ")
  }
}

//case class Context(list: List[Type]) {
//  def +(t: Type): Context = Context(t :: list)
//
//  def apply(k: Int): Type = list(k)
//
//  def length: Int = list.length
//
//  override def toString: String =
//}
//
//object Context {
//  val empty: Context = Context(Nil)
//}
