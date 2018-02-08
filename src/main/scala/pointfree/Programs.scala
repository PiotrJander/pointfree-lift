package pointfree

import Expr.broadcastPredicate

object Programs {

  //  val denseMV: Expr = Map(Reduce(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector) *: (Identity of TList(TFloat) ->: TList(TFloat)))
  val denseMV: Expr = Map(Reduce(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector))

  val csrMV: Expr =
    Map(
      Reduce(Plus) *:
        Map(Uncurry(Mult *: Access(EVector)))
    ) *:
      Map(
        Filter(Neq(Zero) *: Snd) *:
          EZip(Enumeration)
      )

  val segs: Expr = Join *: Map(Tails) *: Inits

  val maxSegSum: Expr = Reduce(Max) *: Map(Reduce(Plus)) *: segs

  val mssHomomorphism: Expr = MssExtract *: Reduce(MssFold) *: Map(MssMap)

  val densePair: Expr =
    Curry(Map(Reduce(Plus)) *: Map(Map(Uncurry(Mult))) *: Map(Uncurry(EZip)) *: Uncurry(EZip)) *: Repeat

  val nonZeroMatrix: Expr =
    broadcastPredicate(broadcastPredicate(Neq(Zero)))

  val denseToBSr: Expr =
    Map(Filter(nonZeroMatrix *: Snd)) *: Map(EZip(Enumeration)) *: Map(Map(Transpose) *: Split *: Transpose) *: Split

  val bsrMV: Expr =
    Join *: Map(Reduce(Lift(Plus)) *: Map(Uncurry(densePair *: Access(Split(EVector)))))

  val bsrConvertThenMV: Expr =
    bsrMV *: denseToBSr
}









