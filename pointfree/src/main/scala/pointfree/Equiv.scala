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
    left = Filter(Neq(Zero)) *: Map(Uncurry(Mult *: A)) *: Rest,
    right = Map(Uncurry(Mult *: A)) *: Filter(Neq(Zero) *: Snd) *: Rest
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
    left = Map(Uncurry(A)) *: EZip(B) *: Rest,
    right = Map(Uncurry(A *: Access(B))) *: EZip(Enumeration) *: Rest
  )

  // max seg sum

  val mapUnDistributesThroughComposition = Equiv(
    name = "map un-distributes through composition",
    left = Map(A) *: Map(B) *: Rest,
    right = Map(A *: B) *: Rest
  )

  val mapPromotion = Equiv(
    name = "map promotion",
    left = Map(A) *: Join *: Rest,
    right = Join *: Map(Map(A)) *: Rest
  )

  val catamorphismPromotion = Equiv(
    name = "catamorphims promotion",
    left = Reduce(A) *: Join *: Rest,
    right = Reduce(A) *: Map(Reduce(A)) *: Rest
  )

  val hornersRule = Equiv(
    name = "Horner's rule",
    left = Reduce(A) *: Map(Reduce(B)) *: Tails *: Rest,
    right = Reduce(B *: A(D)) *: Rest,
    transform = s => neutralElement.get(s(B)).map(neutral => s + (D.n -> neutral))
  )

  val birdsHornersRule = Equiv(
    name = "Bird's Horner's rule",
    left = Foldr(A)(B) *: Tri(C) *: Rest,
    right = Foldr(A)(B *: Bimap(Identity)(C)) *: Rest,
    transform = s => { distributivity.contains((s(C), s(B))).option(s) }
  )

  val foldToScan = Equiv(
    name = "fold to scan",
    left = Map(Reduce(A)) *: Inits *: Rest,
    right = Scan(A) *: Rest
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
    mapUnDistributesThroughComposition,
    mapPromotion,
    catamorphismPromotion,
    hornersRule,
    birdsHornersRule,
    foldToScan
  )

//  def applicableRules(e: Expr): List[(String, Expr, Expr, Expr)] = rewrites.map(e.rewrite2).map(_.toList).concatenate
}





