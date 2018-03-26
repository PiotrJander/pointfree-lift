package pointfree

import org.junit.Test
import pointfree.Equiv._
import pointfree.IdentityEquiv._
import pointfree.Ops._
import pointfree.Programs.{csrMV, denseMV, segs}
import pointfree.TVar._
import util.Random
import scalaz._
import Scalaz._

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
      .tap(le => println(le.head))
      .rewrite(filterSumMonoid)
      .rewrite(mapOverZippedEnumeration)
      .rewrite(filterMapMultAbsorber)
      .identityRewrite(zipUnzip(TInt, TFloat))
      .splitRewrite(splitMapComposition(TList(TFloat) ->: TPair(TList(TInt), TList(TFloat))))
  }

  @Test
  def rewriteMaxSegSum(): Unit = {
    (Programs.maxSegSum :: Nil)
      .tap(le => println(le.head))
      .rewrite(mapPromotion)
      .rewrite(catamorphismPromotion)
      .rewrite(mapUnDistributesThroughComposition)
      .rewrite(mapUnDistributesThroughComposition)
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
  def denseToBsr(): Unit = {
    println(Programs.denseToBSr.typ)
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

  @Test
  def evaluate(): Unit = {
//    val prog = Uncurry(Mult *: Access(EVector))
    val prog = Map(Reduce(Plus) *: Map(Uncurry(Mult *: Access(EVector))))
    val input: List[List[(Int, Float)]] = List(
      List((0, 4), (2, 5)),
      List((1, 6)),
      List((0, 7), (1, 8), (2, 9))
    )
//    val input: (Int, Float) = (2, 1)
    val result = prog.evaluate(input).unwrap.asInstanceOf[List[Float]]
    println(result)
  }

  @Test
  def interpretScan(): Unit = {
    val prog = Scan(Plus)(Zero)
    println(prog.typ)
    val input: List[Float] = (1 to 10).map(_.asInstanceOf[Float]).toList
    val result = prog.evaluate(input).unwrap.asInstanceOf[List[Float]]
    println(result)
  }

  @Test
  def consoleColor(): Unit = {
    import Console.{BLUE, RED, RESET}
    println(s"${BLUE}blue${RESET} default")
  }

  @Test
  def highlights(): Unit = {
    import Console.{BLUE, RED, RESET}
    val prog = Map(Plus) *: Map(Mult) *: Map(And) *: Identity
    val results = prog.rewrite(mapUnDistributesThroughComposition)
    val changes = prog.rewrite2(mapUnDistributesThroughComposition)
    results.zip(changes).foreach({case (result, (lhs, rhs)) =>
      println(prog.toString.replace(lhs.toString, s"$BLUE$lhs$RESET") + "->" +
        result.toString.replace(rhs.toString, s"$RED$rhs$RESET"))
    })
  }

  @Test
  def bsrNew(): Unit = {
    println(Programs.bsrMV.typ)
  }
}










