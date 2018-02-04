package pointfree

object ListExpr {

  implicit class ListExprOps(val le: List[Expr]) {
    def rewrite(equiv: Equiv): List[Expr] = le
      .map(_.normalizeComposition.etaExpansion)
      .flatMap(_ rewrite equiv)
      .map(_.etaReduction.normalizeComposition.print())
      .distinct

    def identityRewrite(ident: IdentityEquiv): List[Expr] = le
      .map(_.normalizeComposition)
      .flatMap(_ identityRewrite ident)
      .map(_.normalizeComposition.etaReduction.print())

    def splitRewrite(equiv: Equiv): List[Expr] = le
      .map(_.normalizeComposition)
      .flatMap(_.compositionAssociativity)
      .flatMap(_ rewrite equiv)
      .map(_.print())

    def tap(f: Expr => Unit): List[Expr] = {
      le.foreach(f(_))
      le
    }

    def typecheck(): List[Expr] = {
      le.foreach(e => println(e.typ))
      le
    }
  }

}
