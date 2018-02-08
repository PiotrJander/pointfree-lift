package pointfree

import EVar._
import Expr._
import Ops._

case class Equiv(left: Expr, right: Expr, transform: Substitution => Option[Substitution]) {
  def transforming(f: Substitution => Option[Substitution]) = Equiv(left, right, f)
}

object Equiv {

  val filterMapMultAbsorber: Equiv =
    Filter(Neq(Zero)) *: Map(Uncurry(Mult *: A)) *: B |≡ Map(Uncurry(Mult *: A)) *: Filter(Neq(Zero) *: Snd) *: B

  val mapDistributivityReverse: Equiv =
    Map(Composition(A, B)) |≡ Composition(Map(A), Map(B))

  def splitMapComposition(t: Type): Equiv =
    Map(Composition(
      //      EA of TPair(TList(TInt), TList(TFloat)) ->: TList(TFloat),
      A,
      B of t
    )) |≡ Composition(Map(A), Map(B))

  val filterSumMonoid: Equiv =
    Reduce(A) |≡ Reduce(A) *: Filter(Neq(B)) transforming
      (s => neutralElement.get(s(A)).map(neutral => s + (B.n -> neutral)))

  val mapOverZippedEnumeration: Equiv =
    Map(Uncurry(A)) *: EZip(B) *: C |≡ Map(Uncurry(A *: Access(B))) *: EZip(Enumeration) *: C

  // max seg sum

  val mapDistributesThroughComposition: Equiv =
    Map(A) *: Map(B) *: C |≡ Map(A *: B) *: C

  val mapPromotion: Equiv =
    Map(A) *: Join *: B |≡ Join *: Map(Map(A)) *: B

  val catamorphismPromotion: Equiv =
    Reduce(A) *: Join *: B |≡ Reduce(A) *: Map(Reduce(A)) *: B

  val hornersRule: Equiv =
    Reduce(A) *: Map(Reduce(B)) *: Tails *: C |≡ Reduce(B *: A(D)) *: C transforming
      (s => neutralElement.get(s(B)).map(neutral => s + (D.n -> neutral)))

  val scan: Equiv =
    Map(Reduce(A)) *: Inits *: B |≡ Scan(A) *: B

  /**
    * This rewrite is only valid for list catamorphisms, not for arbitrary compositions
    * of map reduce.
    *
    * TODO Cole paper: can we obtain the required additional baggage automatically?
    * or just generalize for segment sum problems?
    */
  val catamorphimsPromotion: Equiv =
    Reduce(A) *: Map(B) *: C |≡ Reduce(A) *: Map(Reduce(A) *: Map(B)) *: Split *: C
}





