package pointfree
import EVar._
import TVar._
import Expr._

case class Equiv(left: Expr, right: Expr) {
  def types: (Type, Type) = (left.typ, right.typ)

  def printTypes(): Unit = {
    val (l, r) = types
    println(s"Left: $l right: $r")
  }
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

  private def monoidNeutral(e: Expr): Expr = e match {
    case Plus => Zero
    case Mult => One
  }

  val filterSumMonoid: Equiv =
    Fold(EA) |≡ Fold(EA) *: Filter(Neq(EA.function(monoidNeutral)))

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





