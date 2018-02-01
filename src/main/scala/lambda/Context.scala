package lambda

import scala.collection.immutable

case class Context(map: immutable.Map[Int, Type]) {
  def +(k: Int, t: Type): Context = Context(map + (k -> t))

  def getOrElse(k: Int, t: Type): Type = map.getOrElse(k, t)

  override def toString: String = map.map({ case (k, t) => s"${TVar(k)} : $t" }).mkString(", ")
}

object Context {
  val empty: Context = Context(immutable.Map())
}
