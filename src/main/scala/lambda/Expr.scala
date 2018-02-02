package lambda

import scalaz._
import Scalaz.{tails => _, _}
import scala.collection.immutable
import scala.language.postfixOps
import TVar._
import Var._
import Context._
import com.typesafe.scalalogging.Logger

sealed abstract class Expr {
  import Expr.loggedExpr

  val log = Logger("Expr")

  type Substitution = immutable.Map[Int, Expr]

  def typ(context: Context): Type

  def apply(e: Expr): Expr = Application(this, e)

  def $(e: Expr): Expr = Application(this, e)

  def *:(f: Expr): Expr = a lambda f(this(v0))

  def ++(e: Expr): Expr = Concat(this)(e)

  def toStringDeBruijn(level: Int): String = this.toString

  def logTyping(context: Context, t: Type): Unit = {
    if (!loggedExpr.contains(this)) {
      log.debug(s"${context.toStringContext} |- ${this.toStringDeBruijn(context.length)} : $t")
      loggedExpr += this
    }
  }

  def reduce: Expr = this match {
    case Application(Lambda(t, body), e) => body.substitute(0, e).reduceDeBruijnIndex(0)
    case Application(f, e) => Application(f.reduce, e.reduce)
    case Lambda(t, body) => Lambda(t, body.reduce)
    case _ => this
  }

  def substitute(i: Int, e: Expr): Expr = this match {
    case Application(f, m) => Application(f.substitute(i, e), m.substitute(i, e))
    case Lambda(t, body) => Lambda(t, body.substitute(i + 1, e))
    case Var(n) => if (i == n) e else Var(n)
    case _ => this
  }

  def reduceDeBruijnIndex(i: Int): Expr = this match {
    case Application(f, m) => Application(f.reduceDeBruijnIndex(i), m.reduceDeBruijnIndex(i))
    case Lambda(t, body) => Lambda(t, body.reduceDeBruijnIndex(i + 1))
    case Var(n) => if (n > i) Var(n - 1) else Var(n)
    case _ => this
  }
}

object Expr {

  val loggedExpr: scala.collection.mutable.Set[Expr] = scala.collection.mutable.Set()

  def map(f: Expr): Expr = Catamorphism(Pure *: f)(Concat)

  val flatten: Expr = Catamorphism(Identity)(Concat)

  val initsOp: Expr = TList(TList(a)) lambda (a lambda (v1 ++ Pure(Last(v1) ++ Pure(v0))))

  val inits: Expr = Fold(Pure *: Pure)(initsOp)

  val tailsOp: Expr = TList(TList(a)) lambda (a lambda (map(TList(a) lambda (v0 ++ Pure(v1)))(v1) ++ Pure(Pure(v0))))

  val tails: Expr = Fold(Pure *: Pure)(tailsOp)
}

case class Application(f: Expr, e: Expr) extends Expr {

  override def typ(context: Context): Type = {
    val renaming = f.typ(context) alphaConversion e.typ(context)
    val TArrow(a, b) = f.typ(context) substitute renaming
    val Some(subst) = a unify e.typ(context)
    val res = b substitute subst
    logTyping(context, res)
    res
  }

  override def toStringDeBruijn(level: Int): String = this match {
    case Application(Application(Catamorphism, Identity), Concat) => "flatten"
    case Application(Application(Catamorphism, g), op) => s"⦇${g.toStringDeBruijn(level)}, ${op.toStringDeBruijn(level)}⦈"
    case Application(Pure, e) => s"[${e.toStringDeBruijn(level)}]"
    case Application(Concat, x) => s"${x.toStringDeBruijn(level)} ++"
//    case Application(Application(Catamorphism, Composition(Singleton, g)), Concat) => s"$g*"
//    case Application(Application(Fold, (Composition(Singleton, Singleton))), InitsOp) => "inits"
//    case Application(Application(Fold, (Composition(Singleton, Singleton))), TailsOp) => "tails"
    case _ => s"${f.toStringDeBruijn(level)} ${e.toStringDeBruijn(level)}"
  }

//  def maybeParen(e: Expr): String = e match {
//    case _: Lambda | _: Application => s"($e)"
//    case _ => s"$e"
//  }
}

case class Var(n: Int) extends Expr {
  override def toStringDeBruijn(level: Int): String = ('u' to 'z') (level - n - 1).toString

  override def typ(context: Context): Type = context(n)
}

object Var {
  val v0 = Var(0)
  val v1 = Var(1)
  val v2 = Var(2)
}

case class Lambda(t: Type, body: Expr) extends Expr {
  override def typ(context: Context): Type = t ->: body.typ(t :: context)

  override def toStringDeBruijn(level: Int): String = s"(λ${('u' to 'z')(level)}. ${body.toStringDeBruijn(level + 1)})"
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