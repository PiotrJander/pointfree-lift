package pointfree

import pointfree.EVar._
import pointfree.Expr._
import pointfree.Ops._

import scalaz._
import Scalaz._

case class Equiv(name: String, left: Expr, right: Expr, transform: Substitution => Option[Substitution]) {
  def isApplicable(e: Expr): Boolean = {
//    val Equiv(_, lhs, rhs, transform) = equiv
//    (lhs unify this).flatMap(transform).map(rhs substitute).toList ++ // rewrites this
//      (this match { // recurse
//        case Application(f, e) => combinations(f, e, f rewrite equiv, e rewrite equiv, Application)
//        case Composition(f, g) => combinations(f, g, f rewrite equiv, g rewrite equiv, Composition)
//        case _ => Nil
//      })
    true
  }
}

object Equiv {

  def apply(name: String, left: Expr, right: Expr): Equiv = Equiv(name, left, right, s => Some(s))

  val filterMapMultAbsorber = Equiv(
    name = "filter map mult absorber",
    left = Filter(Neq(Zero)) *: Map(Uncurry(Mult *: A)) *: B,
    right = Map(Uncurry(Mult *: A)) *: Filter(Neq(Zero) *: Snd) *: B
  )

//  val mapDistributivityReverse = Equiv(
//    name = "map distributes backwards through composition, reverse",
//    left = Map(Composition(A, B)),
//    right = Composition(Map(A), Map(B))
//  )

  def splitMapComposition(t: Type) = Equiv(
    name = s"split name composition at type $t",
    left = Map(Composition(A, B of t)),
    right = Composition(Map(A), Map(B))
  )

  val filterSumMonoid = Equiv(
    name = "filter sum monoid",
    left = Reduce(A),
    right = Reduce(A) *: Filter(Neq(B)),
    transform = s => neutralElement.get(s(A)).map(neutral => s + (B.n -> neutral))
  )

  val mapOverZippedEnumeration = Equiv(
    name = "map over zipped enumeration",
    left = Map(Uncurry(A)) *: EZip(B) *: C,
    right = Map(Uncurry(A *: Access(B))) *: EZip(Enumeration) *: C
  )

  // max seg sum

  val mapDistributesThroughComposition = Equiv(
    name = "map distributes backwards through composition",
    left = Map(A) *: Map(B) *: C,
    right = Map(A *: B) *: C
  )

  val mapPromotion = Equiv(
    name = "map promotion",
    left = Map(A) *: Join *: B,
    right = Join *: Map(Map(A)) *: B
  )

  val catamorphismPromotion = Equiv(
    name = "catamorphims promotion",
    left = Reduce(A) *: Join *: B,
    right = Reduce(A) *: Map(Reduce(A)) *: B
  )

  val hornersRule = Equiv(
    name = "Horner's rule",
    left = Reduce(A) *: Map(Reduce(B)) *: Tails *: C,
    right = Reduce(B *: A(D)) *: C,
    transform = s => neutralElement.get(s(B)).map(neutral => s + (D.n -> neutral))
  )

  val birdsHornersRule = Equiv(
    name = "Bird's Horner's rule",
    left = Foldr(A)(B) *: Tri(C) *: D,
    right = Foldr(A)(B *: Bimap(Identity)(C)) *: D,
    transform = s => { distributivity.contains((s(C), s(B))).option(s) }
  )

  val foldToScan = Equiv(
    name = "fold to scan",
    left = Map(Reduce(A)) *: Inits *: B,
    right = Scan(A) *: B
  )

  /**
    * This rewrite is only valid for list catamorphisms, not for arbitrary compositions
    * of map reduce.
    *
    * TODO Cole paper: can we obtain the required additional baggage automatically?
    * or just generalize for segment sum problems?
    */
//  val catamorphismPromotion = Equiv(
//    name = "catamorphism promotion",
//    left = Reduce(A) *: Map(B) *: C,
//    right = Reduce(A) *: Map(Reduce(A) *: Map(B)) *: Split *: C
//  )

  val rewrites = List(
    filterMapMultAbsorber,
    filterSumMonoid,
    mapOverZippedEnumeration,
    mapDistributesThroughComposition,
    mapPromotion,
    catamorphismPromotion,
    hornersRule,
    birdsHornersRule,
    foldToScan
  )

  def applicableRules(e: Expr): List[Equiv] = rewrites.filter(_.isApplicable(e))
}





