package pointfree

//import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.Test
import pointfree.Programs.{csrMV, denseMV}

import scalaz._
import Scalaz.{tails => _, _}

import TVar._
import EVar._
import ListExpr.ListExprOps
import Equiv._
import IdentityEquiv._
import Expr._

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
    println(ex.rewrite(Equiv.mapDistributivity))
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
  def identityRewrite(): Unit = {
    //    println(EA identityRewrite IdentityEquiv.zipUnzip)
    //    println((EA of (TList(TPair(TInt, TFloat)) ->: TList(TPair(TFloat, TInt)))) identityRewrite IdentityEquiv.zipUnzip)
  }

  @Test
  def compositionAssociativity(): Unit = {
    val comp = Plus *: Plus *: Plus *: Plus
    println(comp.compositionAssociativity)
  }

  @Test
  def catamorphismType(): Unit = {
    println(Catamorphism(Identity)(Concat).typ)
  }

  @Test
  def mapPromotion(): Unit = {
    val lhs = map(Placeholder) *: flatten
    println(lhs.etaExpansion.rewrite(catamorphismPromotionLaw).head.etaReduction)
  }

  @Test
  def foldInitsTails(): Unit = {
    println(inits)
    println(inits.typ)
    println(tails)
    println(tails.typ)
  }

  @Test
  def compileSpeed(): Unit = {
    println(flatten.typ)
  }
}










