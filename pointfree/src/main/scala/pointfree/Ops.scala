package pointfree

import Expr.Substitution

object Ops {

  implicit class SubstitutionOps(s: Substitution) {
    def apply(v: EVar): Expr = s(v.n)
  }

  implicit class ListExprOps(le: List[Expr]) {
    def rewrite(equiv: Equiv): List[Expr] = le
      .tap(_ => println(s"= { ${equiv.name} }"))
      .map(_.normalizeComposition.etaExpansion)
      .flatMap(_ rewrite equiv)
      .tap(_ => println(s"= { ${equiv.name} }"))
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

    def tap(f: List[Expr] => Unit): List[Expr] = {
      f(le)
      le
    }

    def typecheck(): List[Expr] = {
      le.foreach(e => println(e.typ))
      le
    }
  }

}
