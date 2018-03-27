package pointfree

import Expr._

object Programs {

  //  val denseMV: Expr = Map(Reduce(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector) *: (Identity of TList(TFloat) ->: TList(TFloat)))
  val denseMV: Expr = Map(Reduce(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector))

  val csrMV: Expr =
    Map(Reduce(Plus) *: Map(Uncurry(Mult *: Access(EVector))))

  val csrConv = Map(Filter(Neq(Zero) *: Snd) *: EZip(Enumeration))

  val bsrMV: Expr =
    Join *: Map(Reduce(ZipWith(Plus)) *: Map(Map(Reduce(Plus) *: Map(Uncurry(Mult))) *: Uncurry(Map *: EZip *: Access(Split(EVector)))))
//    Join *: Map(Map(Map(Reduce(Plus) *: Map(Uncurry(Mult))) *: Uncurry(Map *: EZip *: Access(Split(EVector)))))

  val segs: Expr = Join *: Map(Tails) *: Inits

  val maxSegSum: Expr = Fold(Max)(Zero) *: Map(Fold(Plus)(Zero)) *: segs

//  val mssHomomorphism: Expr = MssExtract *: Reduce(MssFold) *: Map(MssMap)

  val nonZeroMatrix: Expr =
    broadcastPredicate(broadcastPredicate(Neq(Zero)))

  val denseToBSr: Expr =
    Map(Filter(nonZeroMatrix *: Snd)) *: Map(EZip(Enumeration)) *: Map(Map(Transpose) *: Split *: Transpose) *: Split

  val bsrConvertThenMV: Expr =
    bsrMV *: denseToBSr

  val binaryHypeproduct: Expr =
    Fold(Zero)(mult) *: Tri(Square)

  val iai: Expr =
    Fold(Pair(Zero)(Zero))(PlusElemwise) *: Tri(AddSelf) *: Map(Split(Const(Zero))(Identity))
}









