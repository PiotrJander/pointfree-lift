package pointfree

import org.junit.Test
import pointfree.Equiv._
import pointfree.IdentityEquiv._
import pointfree.Ops._
import pointfree.Programs.{csrMV, denseMV}
import pointfree.TVar._

class ExprTest {

  @Test
  def renaming(): Unit = {
    println(A ->: B alphaConversion A ->: B)
  }

  @Test
  def renamingSubstitute(): Unit = {
    val lhs = A ->: B
    val rhs = A ->: B
    println(lhs substitute (lhs alphaConversion rhs))
  }

  @Test
  def unifyDummy(): Unit = {
    val lhs = TPair(A, TFloat)
    val rhs = TPair(TInt, B)
    println(lhs unify rhs)
  }

  @Test
  def unifyDummyFail(): Unit = {
    val lhs = TPair(TFloat, TFloat)
    val rhs = TPair(TInt, B)
    println(lhs unify rhs)
  }

  @Test
  def typecheck(): Unit = {
    println(denseMV.typ)
    println(csrMV.typ)
  }

  @Test
  def normalize(): Unit = {
    val ex = ((Plus *: Plus *: Plus) *: Plus) *: Plus
    println(ex.normalizeComposition)
  }

  @Test
  def etaExpansion(): Unit = {
    println((Plus *: Plus *: Plus).etaExpansion)
    println((Plus *: Plus *: Plus *: Identity).etaExpansion)
  }

    @Test
    def rewriteMapDistributivity(): Unit = {
      val ex = Map(Composition(Plus, Mult))
      println(ex)
      println(ex.rewrite(Equiv.mapDistributivityReverse))
    }

  @Test
  def printPrograms(): Unit = {
    println(Programs.denseMV)
    println(Programs.csrMV)
  }

  @Test
  def typeAnnotation(): Unit = {
    val expr = TypeAnnotation(Map, (TInt ->: TFloat) ->: TList(TInt) ->: TList(TFloat))
    println(expr.typ)
  }

  @Test
  def rewriteCsr(): Unit = {
    (Programs.denseMV :: Nil)
      .rewrite(filterSumMonoid)
      .rewrite(mapOverZippedEnumeration)
      .rewrite(filterMapMultAbsorber)
      .identityRewrite(zipUnzip(TInt, TFloat))
      .splitRewrite(splitMapComposition(TList(TFloat) ->: TPair(TList(TInt), TList(TFloat))))
  }

  @Test
  def rewriteMaxSegSum(): Unit = {
    (Programs.maxSegSum :: Nil)
      .rewrite(mapPromotion)
      .rewrite(catamorphismPromotion)
      .rewrite(mapDistributesThroughComposition)
      .rewrite(mapDistributesThroughComposition)
      .rewrite(hornersRule)
      .rewrite(scan)
      .typecheck()
  }

  @Test
  def rewriteMssPar(): Unit = {
    println(Programs.mssHomomorphism.typ)
  }

  @Test
  def compositionAssociativity(): Unit = {
    val comp = Plus *: Plus *: Plus *: Plus
    println(comp.compositionAssociativity)
  }

}










