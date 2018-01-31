package pointfree

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
}
