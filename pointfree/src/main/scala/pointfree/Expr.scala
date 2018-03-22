package pointfree

import pointfree.TVar._

import scala.collection.immutable
import scala.language.postfixOps
import scalaz.Scalaz._
import scalaz._
import Type._



sealed abstract class Expr {

  import Expr._

  def typ: Type

  def evaluate: Value = VUndefined

  def evaluate(input: Any): Value = this.evaluate match {
    case VFun(f) => f(Value.wrap(input))
  }

  def unify(e: Expr): Option[Substitution] = {
    try {
      (this _unify e).exec(immutable.Map[Int, Expr]()).some
    } catch {
      case UnificationException => none[Substitution]
    }
  }

  def _unify(e: Expr): State[Substitution, Unit] = (this, e) match {
    case (EVar(n), a) => modify(_ + (n -> a))
//    case (TypeAnnotation(expr, t), a) =>
//      t renameAndUnify a.typ match {
//        case Some(_) => expr _unify a
//        case None => throw UnificationException
//      }
    case (Application(a, b), Application(c, d)) => (a _unify c) >> (b _unify d)
    case (Composition(a, b), Composition(c, d)) => (a _unify c) >> (b _unify d)
    case (a, b) => if (a == b) State((_, ())) else throw UnificationException
  }

  def substitute(s: Substitution): Expr = this match {
    case EVar(n) => s(n)
    case Application(f, e) => Application(f substitute s, e substitute s)
    case Composition(f, g) => Composition(f substitute s, g substitute s)
    case _ => this
  }

  def apply(e: Expr): Expr = Application(this, e)

  //  def apply(e1: Expr, e2: Expr) = Application(this, Pair(e1)(e2))

  def *:(f: Expr): Expr = Composition(f, this)

  def of(t: Type): Expr = TypeAnnotation(this, t)

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
    val Equiv(_, lhs, rhs, transform) = equiv
    (lhs unify this).flatMap(transform).map(rhs substitute).toList ++ // rewrites this
      (this match { // recurse
        case Application(f, e) =>
          f.rewrite(equiv).map(Application(_, e)) ++ e.rewrite(equiv).map(Application(f, _))
//          combinations(f, e, f rewrite equiv, e rewrite equiv, Application)
        case Composition(f, g) =>
          f.rewrite(equiv).map(Composition(_, g)) ++ g.rewrite(equiv).map(Composition(f, _))
//          combinations(f, g, f rewrite equiv, g rewrite equiv, Composition)
        case _ => Nil
      })
  }

  def rewrite2(equiv: Equiv): List[(Expr, Expr)] = {
    val Equiv(_, lhs, rhs, transform) = equiv
    (lhs unify this).flatMap(transform).map({ subst =>
      val subst_ = subst + (EVar.Rest.n -> Identity)
      (lhs.substitute(subst_).etaReduction, rhs.substitute(subst_).etaReduction)
    }).toList ++ // rewrites this
      (this match { // recurse
        case Application(f, e) =>
//          combinations2(f, e, f rewrite2 equiv, e rewrite2 equiv, Application)
          f.rewrite2(equiv) ++ e.rewrite2(equiv)
        case Composition(f, g) =>
          f.rewrite2(equiv) ++ g.rewrite2(equiv)
//          combinations2(f, g, f rewrite2 equiv, g rewrite2 equiv, Composition)
        case _ => Nil
      })
  }

  def applyFirst(f: Expr => Expr)(p: (String, Expr, Expr, Expr)): (String, Expr, Expr, Expr) = {
    val (n, a, b, c) = p
    (n, f(a), b, c)
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
  type Substitution = immutable.Map[Int, Expr]

  def broadcastPredicate(p: Expr): Expr = Reduce(And) *: Map(p)

  /**
    * Uncurried functions: names start with lowercase
    */

  val mult: Expr = Uncurry(Mult)

  /**
    * Algebraic properties tables: neutral elements, distributivity, etc
    */

  val distributivity: List[(Expr, Expr)] = List(
    (Square, mult)
  )

  val neutralElement: immutable.Map[Expr, Expr] = immutable.Map(
    Plus -> Zero,
    Mult -> One
  )
}

case class Application(f: Expr, e: Expr) extends Expr {
  override def toString: String = s"${compositionParentheses(f)} ${compositionApplicationParentheses(e)}"

  override def typ: Type = {
    val renaming = f.typ alphaConversion e.typ
    val TArrow(a, b) = f.typ substitute renaming
    val Some(subst) = a unify e.typ
    b substitute subst
  }

  override def evaluate: Value = f.evaluate match {
    case VFun(ff) => ff(e.evaluate)
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

  override def typ: Type = {
    val renaming = f.typ alphaConversion g.typ
    val TArrow(a, b) = g.typ
    val TArrow(b_, c) = f.typ substitute renaming
    val Some(subst) = b unify b_
    (a substitute subst) ->: (c substitute subst)
  }

  override def evaluate: Value = (f.evaluate, g.evaluate) match {
    case (VFun(ff), VFun(gg)) => VFun(arg => ff(gg(arg)))
  }
}

case object Identity extends Expr {
  override def typ: Type = A ->: A

  override def evaluate: Value = VFun(a => a)
}

case object Map extends Expr {
  override def typ: Type = (A ->: B) ->: TList(A) ->: TList(B)

  override def evaluate: Value = VFun {
    case VFun(f) => VFun {
      case VList(list) => VList(list.map(e => f(e)))
    }
  }
}

case object Bimap extends Expr {
  override def typ: Type = (A ->: C) ->: (B ->: D) ->: TPair(A, B) ->: TPair(C, D)
}

case object Tri extends Expr {
  override def typ: Type = (A ->: A) ->: TList(A) ->: TList(A)
}

case object Reduce extends Expr {
  override def typ: Type = (A ->: A ->: A) ->: TList(A) ->: A

  override def evaluate: Value = VFun {
    case VFun(f) => VFun {
      case VList(list) => list.foldRight(VFloat(0).asInstanceOf[Value])((e, acc) => {  // TODO need a start value as a param
        f(e) match {
          case VFun(g) => g(acc)
        }
      })
    }
  }
}

case object Foldr extends Expr {
  override def typ: Type = B ->: (TPair(A, B) ->: B) ->: TList(A) ->: B
}

case object Scan extends Expr {
  override def typ: Type = (A ->: B ->: B) ->: B ->: TList(A) ->: TList(B)

  override def evaluate: Value = VFun { case VFun(f) => VFun { acc => VFun { case VList(xs) => VList(xs.scanLeft(acc)((e, acc) =>
    f(e) match {
    case VFun(g) => g(acc)
  }
  )) } } }
}

case object Filter extends Expr {
  override def typ: Type = (A ->: TBool) ->: TList(A) ->: TList(A)
}

case object Curry extends Expr {
  override def typ: Type = (TPair(A, B) ->: C) ->: A ->: B ->: C
}

case object Uncurry extends Expr {
  override def typ: Type = (A ->: B ->: C) ->: TPair(A, B) ->: C

  override def evaluate: Value = VFun {
    case VFun(f) => VFun {
      case VPair(a, b) => f(a) match {
        case VFun(g) => g(b)
      }
    }
  }
}

case object EZip extends Expr {
  override def typ: Type = TList(A) ->: TList(B) ->: TList(TPair(A, B))

  override def evaluate: Value = VFun { case VList(xs) => VFun { case VList(ys) => VList(xs.zip(ys).map({case (a, b) => VPair(a, b)})) } }
}

case object Unzip extends Expr {
  override def typ: Type = TList(TPair(A, B)) ->: TPair(TList(A), TList(B))
}

case object Access extends Expr {
  override def typ: Type = TList(A) ->: TInt ->: A

  override def evaluate: Value = VFun {
    case VList(list) => VFun {
      case VInt(i) => list(i)
//      case _ => VUndefined
    }
//    case _ => VUndefined
  }
}

case object Neq extends Expr {
  override def typ: Type = A ->: A ->: TBool
}

case object Snd extends Expr {
  override def typ: Type = TPair(A, B) ->: B
}

case object And extends Expr {
  override def typ: Type = TBool ->: TBool ->: TBool
}

case object Plus extends Expr {
  override def typ: Type = TFloat ->: TFloat ->: TFloat

  override def evaluate: Value = VFun {
    case VFloat(x) => VFun {
      case VFloat(y) => VFloat(x + y)
//      case _ => VUndefined
    }
//    case _ => VUndefined
  }
}

case object Mult extends Expr {
  override def typ: Type = TFloat ->: TFloat ->: TFloat

  override def evaluate: Value = VFun {
    case VFloat(x) => VFun {
      case VFloat(y) => VFloat(x * y)
      //      case _ => VUndefined
    }
    //    case _ => VUndefined
  }
}

case object Max extends Expr {
  override def typ: Type = TFloat ->: TFloat ->: TFloat

  override def evaluate: Value = VFun { case VFloat(x) => VFun { case VFloat(y) => VFloat(x max y) }}
}

case object Zero extends Expr {
  override def typ: Type = TFloat
  override def evaluate: Value = VFloat(0)
}

case object One extends Expr {
  override def typ: Type = TFloat
  override def evaluate: Value = VFloat(1)
}

case object EVector extends Expr {

  val value: VList = VList(List.fill(1000)(util.Random.nextFloat()).map(VFloat))

  override def typ: Type = TList(TFloat)
  override def evaluate: Value = value
}

case object Enumeration extends Expr {
  override def typ: Type = TList(TInt)
}

case object Inits extends Expr {
  override def typ: Type = TList(A) ->: TList(TList(A))
  override def evaluate: Value = VFun { case VList(xs) => VList(initz(xs).map(VList)) }
}

case object Tails extends Expr {
  override def typ: Type = TList(A) ->: TList(TList(A))
  override def evaluate: Value = VFun { case VList(xs) => VList(tailz(xs).map(VList)) }
}

case object Join extends Expr {
  override def typ: Type = TList(TList(A)) ->: TList(A)
  override def evaluate: Value = VFun { case VList(xs) => VList(xs.map({ case VList(ys) => ys }).concatenate) }
}

case object Split extends Expr {
  override def typ: Type = TList(A) ->: TList(TList(A))
}

case object PairSplit extends Expr {
  override def typ: Type = (A ->: B) ->: (A ->: C) ->: A ->: TPair(B, C)
}

case object Repeat extends Expr {
  override def typ: Type = A ->: TList(A)
}

case object Lift extends Expr {
  override def typ: Type = (A ->: A ->: A) ->: TList(A) ->: TList(A) ->: TList(A)
}

case object FMap extends Expr {
  override def typ: Type = (A ->: B) ->: TPair(A, C) ->: TPair(B, C)
}

case object SMap extends Expr {
  override def typ: Type = (A ->: B) ->: TPair(C, A) ->: TPair(C, B)
}

case object Transpose extends Expr {
  override def typ: Type = TList(TList(A)) ->: TList(TList(A))
}

case object Square extends Expr {
  override def typ: Type = TFloat ->: TFloat
}

case object Pair extends Expr {
  override def typ: Type = A ->: B ->: TPair(A, B)
}

case object Const extends Expr {
  override def typ: Type = A ->: B ->: A
}

case object AddSelf extends Expr {
  override def typ: Type = TPair(TFloat, TFloat) ->: TPair(TFloat, TFloat)
}

case object PlusElemwise extends Expr {
  override def typ: Type = TPair(TPair(TFloat, TFloat), TPair(TFloat, TFloat)) ->: TPair(TFloat, TFloat)
}

case class TypeAnnotation(e: Expr, t: Type) extends Expr {
  override def toString: String = s"($e :: $t)"

  override def typ: Type = {
    val typ = e.typ
    val renaming = typ alphaConversion t
    val typRenamed = typ substitute renaming
    val Some(subst) = typRenamed unify t
    t substitute subst
  }
}

case class EVar(n: Int) extends Expr {
  override def toString: String = if (n == EVar.Placeholder.n) "Placeholder" else super.toString

  override def typ: Type = A
}

object EVar {
  val A = EVar(0)
  val B = EVar(1)
  val C = EVar(2)
  val D = EVar(3)
  val E = EVar(4)
  val Rest = EVar(5)
  val Placeholder = EVar(6)
}
