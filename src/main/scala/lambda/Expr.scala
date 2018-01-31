package lambda

import scalaz._
import Scalaz.{tails => _, _}
import scala.collection.immutable
import scala.language.postfixOps
import Type._
import TVar._
import Var._
import pointfree.Expr

import scalaz.Alpha.A

sealed abstract class Expr {

  type Substitution = immutable.Map[Int, Expr]

  def typ(context: Context): Type

  def apply(e: Expr): Expr = Application(this, e)

  def &:(e: Expr): Expr = Application(e, this)

  def *:(f: Expr): Expr = u of a lambda this(f(u))

  def ++(e: Expr): Expr = Concat(this)(e)
}

object Expr {
  def map(f: Expr): Expr = Catamorphism(Pure *: f)(Concat)

  val flatten: Expr = Catamorphism(Identity)(Concat)

  val initsOp: Expr = u of TList(TList(a)) lambda (v of a lambda (u ++ Pure(Last(u) ++ Pure(v))))

  val inits: Expr = Fold(Pure *: Pure)(initsOp)

  val tailsOp: Expr = u of TList(TList(a)) lambda (v of a lambda (map(w lambda (w ++ Pure(v)))(u) ++ Pure(Pure(v))))

  val tails: Expr = Fold(Pure *: Pure)(tailsOp)
}

case class Application(f: Expr, e: Expr) extends Expr {

  override def typ(context: Context): Type = {
    val renaming = f.typ(context) alphaConversion e.typ(context)
    val TArrow(a, b) = f.typ(context) substitute renaming
    val Some(subst) = a unify e.typ(context)
    b substitute subst
  }

  override def toString: String = this match {
    case Application(Application(Catamorphism, g), op) => s"⦇$g, $op⦈"
    case Application(Application(Catamorphism, Identity), Concat) => "flatten"
//    case Application(Application(Catamorphism, Composition(Singleton, g)), Concat) => s"$g*"
//    case Application(Application(Fold, (Composition(Singleton, Singleton))), InitsOp) => "inits"
//    case Application(Application(Fold, (Composition(Singleton, Singleton))), TailsOp) => "tails"
    case _ => s"$f $e"
  }
}

case class Var(n: Int) extends Expr {
  override def toString: String = ('a' to 'z') (n).toString

  override def typ(context: Context): Type = context.getOrElse(n, a)

  def of(t: Type): VarType = VarType(this, t)

  def lambda(body: Expr) = Lambda(this, a, body)
}

object Var {
  val u = Var(20)
  val v = Var(21)
  val w = Var(22)
}

case class VarType(v: Var, t: Type) {
  def lambda(e: Expr): Lambda = Lambda(v, t, e)
}

case class Lambda(v: Var, t: Type, body: Expr) extends Expr {
  override def typ(context: Context): Type = t ->: body.typ(context.updated(v.n, t))

  override def toString: String = s"λ$v. $body"
}

case object Identity extends Expr {
  override def typ(context: Context): Type = a ->: a
}

case class Pattern(n: Int) extends Expr {
  override def toString: String = ('A' to 'Z') (n).toString

  override def typ(context: Context): Type = ???
}

object Pattern {
  val A = Pattern(0)
  val B = Pattern(1)
  val C = Pattern(2)
  val D = Pattern(3)
}

case object Catamorphism extends Expr {
  override def typ(context: Context): Type = (a ->: b) ->: (b ->: b ->: b) ->: TList(a) ->: b
}

case object Fold extends Expr {
  override def typ(context: Context): Type = (a ->: b) ->: (b ->: a ->: b) ->: TList(a) ->: b
}

case object Scan extends Expr {
  override def typ(context: Context): Type = (a ->: b) ->: (b ->: a ->: b) ->: TList(a) ->: TList(b)
}

case object Placeholder extends Expr {
  override def typ(context: Context): Type = a ->: b
}

case object Concat extends Expr {
  override def typ(context: Context): Type = TList(a) ->: TList(a) ->: TList(a)
}

case object Last extends Expr {
  override def typ(context: Context): Type = TList(a) ->: a
}

case object Pure extends Expr {
  override def typ(context: Context): Type = a ->: TList(a)
}