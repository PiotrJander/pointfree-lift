package pointfree

import scalaz._
import Scalaz.{tails => _, _}
import scala.collection.immutable
import scala.language.postfixOps
import TVar._

sealed abstract class Expr {

  def typ: Type = this match {
    case Application(f, e) =>
      val renaming = f.typ alphaConversion e.typ
      val TArrow(a, b) = f.typ substitute renaming
      val Some(subst) = a unify e.typ
      b substitute subst
    case Composition(f, g) =>
      val renaming = f.typ alphaConversion g.typ
      val TArrow(a, b) = g.typ
      val TArrow(b_, c) = f.typ substitute renaming
      val Some(subst) = b unify b_
      (a substitute subst) ->: (c substitute subst)
    case TypeAnnotation(e, t) =>
      val typ = e.typ
      val renaming = typ alphaConversion t
      val typRenamed = typ substitute renaming
      val Some(subst) = typRenamed unify t
      t substitute subst
    case Catamorphism => (A ->: B) ->: (B ->: B ->: B) ->: TList(A) ->: B
    case Fold => (A ->: B) ->: (B ->: A ->: B) ->: TList(A) ->: B
    case Scan => (A ->: B) ->: (B ->: A ->: B) ->: TList(A) ->: TList(B)
    case Placeholder => A ->: B
    case Identity => A ->: A
    case Concat => TList(A) ->: TList(A) ->: TList(A)
    case Singleton => A ->: TList(A)
    case Map => (A ->: B) ->: TList(A) ->: TList(B)
    case Reduce => (A ->: A ->: A) ->: TList(A) ->: A
    case Filter => (A ->: TBool) ->: TList(A) ->: TList(A)
    case Uncurry => (A ->: B ->: C) ->: TPair(A, B) ->: C
    case EZip => TList(A) ->: TList(B) ->: TList(TPair(A, B))
    case Unzip => TList(TPair(A, B)) ->: TPair(TList(A), TList(B))
    case Access => TList(A) ->: TInt ->: A
    case Neq => A ->: A ->: TBool
    case Snd => TPair(A, B) ->: B
    case Plus => TFloat ->: TFloat ->: TFloat
    case Mult => TFloat ->: TFloat ->: TFloat
    case Zero => TFloat
    case One => TFloat
    case EVector => TList(TFloat)
    case Enumeration => TList(TInt)
    case _: Variable => A
    case _ => this.typ
  }

  type Substitution = immutable.Map[Int, Expr]

  def unify(e: Expr): Option[Substitution] = {
    try {
      (this _unify e).exec(immutable.Map[Int, Expr]()).some
    } catch {
      case UnificationException => none[Substitution]
    }
  }

  def _unify(e: Expr): State[Substitution, Unit] = (this, e) match {
    case (Variable(n), a) => modify(_ + (n -> a))
    case (TypeAnnotation(expr, t), a) =>
      t renameAndUnify a.typ match {
        case Some(_) => expr _unify a
        case None => throw UnificationException
      }
    case (Application(a, b), Application(c, d)) => (a _unify c) >> (b _unify d)
    case (Composition(a, b), Composition(c, d)) => (a _unify c) >> (b _unify d)
    case (a, b) => if (a == b) State((_, ())) else throw UnificationException
  }

  def substitute(s: Substitution): Expr = this match {
    case EVar(n) => s(n)
    case FunctionVar(n, f) => f(s(n))
    case Application(f, e) => Application(f substitute s, e substitute s)
    case Composition(f, g) => Composition(f substitute s, g substitute s)
    case _ => this
  }

  def apply(e: Expr): Expr = Application(this, e)

  def *:(f: Expr): Expr = Composition(f, this)

  def of(t: Type): Expr = TypeAnnotation(this, t)

  def |≡(expr: Expr): Equiv = Equiv(this, expr)

  def print(): Expr = {
    println(this)
    this
  }

  def normalizeComposition: Expr = this match {
    case Composition(Composition(f, g), h) => (f.normalizeComposition *: (g *: h).normalizeComposition).normalizeComposition
    case Composition(f, g) => f.normalizeComposition *: g.normalizeComposition
    case Application(f, e) => f.normalizeComposition.apply(e.normalizeComposition)
    case _ => this
  }

  /**
    * We assume that the expression is normalized.
    * TODO phantom types
    */
  def etaExpansion: Expr = this match {
    case Composition(f, Composition(g, h)) => f.etaExpansion *: (g *: h).etaExpansion
    case Composition(f, Identity) => Composition(f.etaExpansion, Identity)
    case Composition(f, g) => f.etaExpansion *: g.etaExpansion *: Identity
    case Application(f, e) => f.etaExpansion.apply(e.etaExpansion)
    case _ => this
  }

  def etaReduction: Expr = this match {
    case Composition(f, Identity) => f.etaReduction
    case Composition(f, g) => f.etaReduction *: g.etaReduction
    case Application(f, e) => f.etaReduction.apply(e.etaReduction)
    case _ => this
  }

  def combinations[A](a: A, b: A, xs: List[A], ys: List[A], f: (A, A) => A): List[A] =
    (for {x <- xs; y <- ys} yield f(x, y)) ++ (for {x <- xs} yield f(x, b)) ++ (for {y <- ys} yield f(a, y))

  /**
    * Non-deterministic rewrite. Returns a list of all possible applications of the re-write.
    */
  def rewrite(equiv: Equiv): List[Expr] = {
    val Equiv(lhs, rhs) = equiv
    (lhs unify this).map(rhs substitute).toList ++ // rewrites this
      (this match { // recurse
        case Application(f, e) => combinations(f, e, f rewrite equiv, e rewrite equiv, Application)
        case Composition(f, g) => combinations(f, g, f rewrite equiv, g rewrite equiv, Composition)
        case _ => Nil
      })
  }

  def identityRewrite(ident: IdentityEquiv): List[Expr] = (this _identityRewrite ident) ++ (this match {
    case Application(f, e) => combinations(f, e, f identityRewrite ident, e identityRewrite ident, Application)
    case Composition(f, g) => combinations(f, g, f identityRewrite ident, g identityRewrite ident, Composition)
    case _ => Nil
  })

  def _identityRewrite(ident: IdentityEquiv): List[Expr] = this.typ match {
    case TArrow(_, b) =>
      val bUnifies = ident.t renameAndStrongUnify b
      bUnifies.map(_ => ident.e *: this).toList
    case _ =>
      Nil
  }

  def compositionAssociativity: List[Expr] = (this match {
    case Application(f, e) => for {
      f_ <- f.compositionAssociativity
      e_ <- e.compositionAssociativity
    } yield f_.apply(e_)
    case Composition(f, g) => this :: (for {
      f_ <- f.compositionAssociativity
      g_ <- g.compositionAssociativity
    } yield g_ match {
      case Composition(h, i) => (f_ *: h) *: i
      case _ => f_ *: g_
    })
    case _ => this :: Nil
  }).distinct
}

object Expr {
  def map(f: Expr): Expr = Catamorphism(Singleton *: f)(Concat)

  val flatten: Expr = Catamorphism(Identity)(Concat)

  val inits: Expr = Fold(Singleton *: Singleton)(InitsOp)

  val tails: Expr = Fold(Singleton *: Singleton)(TailsOp)
}

case class Application(f: Expr, e: Expr) extends Expr {
  override def toString: String = this match {
    case Application(Application(Catamorphism, Identity), Concat) => "flatten"
    case Application(Application(Catamorphism, Composition(Singleton, g)), Concat) => s"$g*"
    case Application(Application(Catamorphism, g), op) => s"(|$g, $op|)"
    case Application(Application(Fold, (Composition(Singleton, Singleton))), InitsOp) => "inits"
    case Application(Application(Fold, (Composition(Singleton, Singleton))), TailsOp) => "tails"
    case _ => s"${compositionParentheses(f)} ${compositionApplicationParentheses(e)}"
  }

  def compositionParentheses(c: Expr): String = c match {
    case c: Composition => s"($c)"
    case _ => c.toString
  }

  def compositionApplicationParentheses(c: Expr): String = c match {
    case c: Application => s"($c)"
    case c: Composition => s"($c)"
    case _ => c.toString
  }
}

case class Composition(f: Expr, g: Expr) extends Expr {
  override def toString: String = f match {
    case _: Composition => s"($f) ∘ $g"
    case _ => s"$f ∘ $g"
  }
}

case object Identity extends Expr

case object Map extends Expr

case object Reduce extends Expr

case object Filter extends Expr

case object Uncurry extends Expr

case object EZip extends Expr

case object Unzip extends Expr

case object Access extends Expr

case object Neq extends Expr

case object Snd extends Expr

case object Plus extends Expr

case object Mult extends Expr

case object Zero extends Expr

case object One extends Expr

case object EVector extends Expr

case object Enumeration extends Expr

case object Catamorphism extends Expr

case object Fold extends Expr

case object Scan extends Expr

case object Placeholder extends Expr

case object Concat extends Expr

case object Singleton extends Expr

/**
  * foldrOp :: [a] -> b -> [a]
  * initsOp :: [[ c ]] -> d -> [[ c ]]
  * initsOp u a = u ++ pure (last u ++ pure a)
  */
case object InitsOp extends Expr {
  override def typ: Type = TList(TList(A)) ->: B ->: TList(TList(A))
}

/**
  * foldrOp :: [a] -> b -> [a]
  * tailsOp :: [[ c ]] -> d -> [[ c ]]
  * tailsOp z a = map (++ pure a) z ++ pure (pure a)
  */
case object TailsOp extends Expr {
  override def typ: Type = TList(TList(A)) ->: B ->: TList(TList(A))
}

case class TypeAnnotation(e: Expr, t: Type) extends Expr {
  override def toString: String = s"($e :: $t)"
}

trait Variable {
  val m: Int

  override def toString: String = ('A' to 'Z') (m).toString
}

object Variable {
  def unapply(v: Variable): Option[Int] = v.m.some
}

case class EVar(n: Int) extends Expr with Variable {
  override val m: Int = n

  def function(f: Expr => Expr): FunctionVar = FunctionVar(n, f)
}

case class FunctionVar(n: Int, f: Expr => Expr) extends Expr with Variable {
  override val m: Int = n
}

object EVar {
  val EA = EVar(0)
  val EB = EVar(1)
  val EC = EVar(2)
  val ED = EVar(3)
}
