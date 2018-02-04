package pointfree

import EVar._
import Expr._
import Ops._

case class Equiv(left: Expr, right: Expr, transform: Substitution => Option[Substitution]) {
  def transforming(f: Substitution => Option[Substitution]) = Equiv(left, right, f)
}

object Equiv {

  val filterMapMultAbsorber: Equiv =
    Filter(Neq(Zero)) *: Map(Uncurry(Mult *: EA)) *: EB |≡ Map(Uncurry(Mult *: EA)) *: Filter(Neq(Zero) *: Snd) *: EB

  val mapDistributivityReverse: Equiv =
    Map(Composition(EA, EB)) |≡ Composition(Map(EA), Map(EB))

  def splitMapComposition(t: Type): Equiv =
    Map(Composition(
      //      EA of TPair(TList(TInt), TList(TFloat)) ->: TList(TFloat),
      EA,
      EB of t
    )) |≡ Composition(Map(EA), Map(EB))

  val filterSumMonoid: Equiv =
    Fold(EA) |≡ Fold(EA) *: Filter(Neq(EB)) transforming
      (s => neutralElement.get(s(EA)).map(neutral => s + (EB.n -> neutral)))

  val mapOverZippedEnumeration: Equiv =
    Map(Uncurry(EA)) *: EZip(EB) *: EC |≡ Map(Uncurry(EA *: Access(EB))) *: EZip(Enumeration) *: EC

  // max seg sum

  val mapDistributesThroughComposition: Equiv =
    Map(EA) *: Map(EB) *: EC |≡ Map(EA *: EB) *: EC

  val mapPromotion: Equiv =
    Map(EA) *: Join *: EB |≡ Join *: Map(Map(EA)) *: EB

  val catamorphismPromotion: Equiv =
    Fold(EA) *: Join *: EB |≡ Fold(EA) *: Map(Fold(EA)) *: EB

}





