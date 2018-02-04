package pointfree

object Programs {

  //  val denseMV: Expr = Map(Reduce(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector) *: (Identity of TList(TFloat) ->: TList(TFloat)))
  val denseMV: Expr = Map(Fold(Plus) *: Map(Uncurry(Mult)) *: EZip(EVector))

  val csrMV: Expr =
    Map(
      Fold(Plus) *:
        Map(Uncurry(Mult *: Access(EVector)))
    ) *:
      Map(
        Filter(Neq(Zero) *: Snd) *:
          EZip(Enumeration)
      )

  val segs: Expr = Join *: Map(Tails) *: Inits

  val maxSegSum: Expr = Fold(Max) *: Map(Fold(Plus)) *: segs
}
