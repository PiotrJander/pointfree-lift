package pointfree

import scalaz.{State, _}
import Scalaz._
import scala.collection.immutable


sealed abstract class Type {

  type Substitution = immutable.Map[Int, Type]

  def ->:(a: Type): Type = TArrow(a, this)

  def typeVariables(): List[Int] = this match {
    case TList(a) => a.typeVariables()
    case TPair(a, b) => a.typeVariables() ++ b.typeVariables()
    case TArrow(a, b) => a.typeVariables() ++ b.typeVariables()
    case TVar(n) => n :: Nil
    case _ => Nil
  }

  def alphaConversion(rhs: Type): Substitution = {
    val lhsVars = this.typeVariables().toSet
    val rhsVars = rhs.typeVariables().toSet
    val allVars = lhsVars union rhsVars
    val repeatedVars = lhsVars intersect rhsVars
    _alpha(allVars.toList, repeatedVars.toList)
  }

  def _alpha(all: List[Int], repeated: List[Int]): Substitution = repeated match {
    case Nil => immutable.Map[Int, Type]()
    case v :: rest =>
      val free: Int = (0 until Int.MaxValue).find(!all.contains(_)).get
      _alpha(free :: all, rest) + (v -> TVar(free))
  }

  def substitute(subst: Substitution): Type = this match {
    case TList(a) => TList(a substitute subst)
    case TPair(a, b) => TPair(a substitute subst, b substitute subst)
    case TArrow(a, b) => TArrow(a substitute subst, b substitute subst)
    case TVar(n) => subst.getOrElse(n, this)
    case _ => this
  }

  def unify(t: Type): Option[Substitution] =
    try {
      (this _unify t).exec(immutable.Map[Int, Type]()).some
    } catch {
      case UnificationException => None
    }

  def strongUnify(t: Type): Option[Substitution] =
    try {
      (this _strongUnify t).exec(immutable.Map[Int, Type]()).some
    } catch {
      case UnificationException => None
    }

  def _unify(t: Type): State[Substitution, Unit] = (this, t) match {
    // TODO impl proper unification
    case (TVar(n), a) => modify(s => s + (n -> a))
    case (a, TVar(n)) => modify(s => s + (n -> a))
    case (TList(a), TList(b)) => a _unify b
    case (TPair(a, b), TPair(c, d)) => (a _unify c) >> (b _unify d)
    case (TArrow(a, b), TArrow(c, d)) => (a _unify c) >> (b _unify d)
    case (a, b) => if (a == b) State(s => (s, ())) else throw UnificationException
  }

  /**
    * Like unify, but when we unify a with b, ...
    */
  def _strongUnify(t: Type): State[Substitution, Unit] = (this, t) match {
    // TODO impl proper unification
    case (TVar(n), a) => modify(s => s + (n -> a))
    case (TList(a), TList(b)) => a _strongUnify b
    case (TPair(a, b), TPair(c, d)) => (a _strongUnify c) >> (b _strongUnify d)
    case (TArrow(a, b), TArrow(c, d)) => (a _strongUnify c) >> (b _strongUnify d)
    case (a, b) => if (a == b) State(s => (s, ())) else throw UnificationException
  }

  def renameAndUnify(t: Type): Option[Substitution] = {
    val renaming = this alphaConversion t
    val tRenamed = t substitute renaming
    this unify tRenamed
  }

  def renameAndStrongUnify(t: Type): Option[Substitution] = {
    val renaming = this alphaConversion t
    val tRenamed = t substitute renaming
    this strongUnify tRenamed
  }

  def |-(e: Expr) = IdentityEquiv(this, e)

  override def toString: String = this match {
    case TInt => "Int"
    case TFloat => "Float"
    case TBool => "Bool"
    case TList(a) => s"[$a]"
    case TPair(a, b) => s"($a, $b)"
    case TArrow(a, b) => a match {
      case _: TArrow => s"($a) → $b"
      case _ => s"$a → $b"
    }
    case TVar(n) => ('a' to 'z') (n).toString
  }
}

object Type {
  def Quad(a: Type, b: Type, c: Type, d: Type): Type = TPair(a, TPair(b, TPair(c, d)))
}

case object TInt extends Type

case object TFloat extends Type

case object TBool extends Type

case class TList(a: Type) extends Type

case class TPair(a: Type, b: Type) extends Type

case class TArrow(a: Type, b: Type) extends Type

case class TVar(n: Int) extends Type

object TVar {
  val A = TVar(0)
  val B = TVar(1)
  val C = TVar(2)
  val D = TVar(3)
}
