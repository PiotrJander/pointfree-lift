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
      .tap(println)
      .rewrite(mapPromotion)
      .rewrite(catamorphismPromotion)
      .rewrite(mapDistributesThroughComposition)
      .rewrite(mapDistributesThroughComposition)
      .rewrite(hornersRule)
      .rewrite(foldToScan)
      .typecheck()
  }

//  @Test
//  def rewriteMssPar(): Unit = {
//    println(Programs.mssHomomorphism.typ)
//    (Programs.mssHomomorphism :: Nil)
//      .rewrite(catamorphimsPromotion)
//      .rewrite(catamorphimsPromotion)
//      .typecheck()
//  }

  @Test
  def compositionAssociativity(): Unit = {
    val comp = Plus *: Plus *: Plus *: Plus
    println(comp.compositionAssociativity)
  }

  @Test
  def densePair(): Unit = {
    println(Programs.densePair.typ)
  }

  @Test
  def denseToBsr(): Unit = {
    println(Programs.denseToBSr.typ)
  }

  @Test
  def bsrMV(): Unit = {
    println(Programs.bsrMV.typ)
  }

  @Test
  def binHyperprod(): Unit = {
    println(Programs.binaryHypeproduct.typ)
    (Programs.binaryHypeproduct :: Nil)
        .tap(println)
        .rewrite(birdsHornersRule)
        .typecheck()
  }

  @Test
  def iai(): Unit = {
    println(Programs.iai.typ)
  }


}










