package lambda

import pointfree.UnificationException

import scalaz.{State, _}
import Scalaz.{tails => _, _}
import scala.collection.immutable


sealed abstract class Type {

  def ->:(a: Type): Type = TArrow(a, this)

//  def typeVariables(): List[Int] = this match {
//    case TList(a) => a.typeVariables()
//    case TPair(a, b) => a.typeVariables() ++ b.typeVariables()
//    case TArrow(a, b) => a.typeVariables() ++ b.typeVariables()
//    case TVar(n) => n :: Nil
//    case _ => Nil
//  }

//  def alphaConversion(rhs: Type): Context = {
//    val lhsVars = this.typeVariables().toSet
//    val rhsVars = rhs.typeVariables().toSet
//    val allVars = lhsVars union rhsVars
//    val repeatedVars = lhsVars intersect rhsVars
//    _alpha(allVars.toList, repeatedVars.toList)
//  }

//  def _alpha(all: List[Int], repeated: List[Int]): Context = repeated match {
//    case Nil => immutable.Map()
//    case v :: rest =>
//      val free: Int = (0 until Int.MaxValue).find(!all.contains(_)).get
//      _alpha(free :: all, rest) + (v -> TVar(free))
//  }

//  def substitute(subst: Context): Type = this match {
//    case TList(a) => TList(a substitute subst)
//    case TPair(a, b) => TPair(a substitute subst, b substitute subst)
//    case TArrow(a, b) => TArrow(a substitute subst, b substitute subst)
//    case TVar(n) => subst.getOrElse(n, this)
//    case _ => this
//  }

//  def unify(t: Type): Option[Context] =
//    try {
//      (this _unify t).exec(immutable.Map()).some
//    } catch {
//      case UnificationException => None
//    }

//  def _unify(t: Type): State[Context, Unit] = (this, t) match {
//    // TODO impl proper unification
//    case (TVar(n), a) => modify(s => s + (n -> a))
//    case (a, TVar(n)) => modify(s => s + (n -> a))
//    case (TList(a), TList(b)) => a _unify b
//    case (TPair(a, b), TPair(c, d)) => (a _unify c) >> (b _unify d)
//    case (TArrow(a, b), TArrow(c, d)) => (a _unify c) >> (b _unify d)
//    case (a, b) => if (a == b) State(s => (s, ())) else throw UnificationException
//  }

//  def renameAndUnify(t: Type): Option[Substitution] = {
//    val renaming = this alphaConversion t
//    val tRenamed = t substitute renaming
//    this unify tRenamed
//  }

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

  def lambda(body: Expr): Lambda = Lambda(this, body)
}

object Type {
  type TypeEnv = immutable.Map[Var, Scheme]  // TODO
  type Subst = immutable.Map[TVar, Type]

}

case object TInt extends Type

case object TFloat extends Type

case object TBool extends Type

case class TList(a: Type) extends Type

case class TPair(a: Type, b: Type) extends Type

case class TArrow(a: Type, b: Type) extends Type

case class TVar(n: Int) extends Type

object TVar {
  val a = TVar(0)
  val b = TVar(1)
  val c = TVar(2)
  val d = TVar(3)
}

