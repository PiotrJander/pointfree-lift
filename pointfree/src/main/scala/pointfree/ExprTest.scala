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
  def mssBenchmark(): Unit = {
    val spec = Programs.maxSegSum
    val opt = Reduce(Max) *: Scan(Plus *: Max(Zero))(Zero)
//    val input: List[Float] = (1 to 10).map(_.asInstanceOf[Float]).toList
    val input = List.fill(500)(util.Random.nextFloat())
    val (specTime, specResult) = profile(spec.evaluate(input).unwrap.asInstanceOf[Float])
    val (optTime, optResult) = profile(opt.evaluate(input).unwrap.asInstanceOf[Float])
    println(s"$specResult ${specTime / 1e+9}")
    println(s"$optResult ${optTime / 1e+9}")
  }

  def generateDense(m: Int, n: Int, density: Int): Vector[Vector[Float]] =
    Vector.fill(m)(Vector.fill(m)(if (util.Random.nextInt(density) == 0) util.Random.nextFloat() else 0))

  def csrFromDense(dense: Vector[Vector[Float]]): Vector[Vector[(Int, Float)]] =
    dense.map(row => row
      .zipWithIndex
      .filter({case (v, i) => i.abs > 1e-6 })
      .map({ case (v, i) => (i, v) }))

  def generateBsr(bm: Int, bn: Int, size: Int, blockDensity: Int, density: Int): Vector[Vector[(Int, Vector[Vector[Float]])]] =
    Vector.fill(bm)(Vector.fill(bn)(Random.nextInt(blockDensity)).zipWithIndex.filter(_._1 == 0).map({ case (_, i) =>
      (i, Vector.fill(size)(Vector.fill(size)(if (Random.nextInt(density) == 0) Random.nextFloat() else 0))) }))

  def denseFromBsr(size: Int, bsr: Vector[Vector[(Int, Vector[Vector[Float]])]]): Vector[Vector[Float]] =
    bsr.map(blockRow => fromBsrRow(size, 0, blockRow).transpose.map(_.concatenate)).concatenate

  def zeroBlock(size: Int): Vector[Vector[Float]] = Vector.fill(size)(Vector.fill(size)(0))

  def fromBsrRow(size: Int, i: Int, blockRow: Vector[(Int, Vector[Vector[Float]])]): Vector[Vector[Vector[Float]]] = blockRow match {
    case (j, block) +: rest => if (j == i) block +: fromBsrRow(size, i + 1, rest) else zeroBlock(size) +: fromBsrRow(size, i + 1, (j, block) +: rest)
    case _ => if (i == 10) Vector() else zeroBlock(size) +: fromBsrRow(size, i + 1, Vector())
  }

  def bsrFromDense(blockSize: Int, dense: Vector[Vector[Float]]): Vector[Vector[(Int, Vector[Vector[Float]])]] =
    dense.grouped(blockSize).map(_.grouped(blockSize).toVector).toVector.map(_.transpose.zipWithIndex.map(_.swap))

  def profile[A](f: => A): (Long, A) = {
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    (end - start, result)
  }

  @Test
  def csrBenchmark(): Unit = {
    val denseInput: Vector[Vector[Float]] = generateDense(1000, 1000, 10)
    val csrInput: Vector[Vector[(Int, Float)]] = csrFromDense(denseInput)

    val (denseTime, denseRes) = profile(Programs.denseMV.evaluate(denseInput))
    val (csrTime, csrRes) = profile(Programs.csrMV.evaluate(csrInput))

    println(s"dense ${denseRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${denseTime / 1e+9}")
    println(s"csr ${csrRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${csrTime / 1e+9}")
  }

  @Test
  def bsrBenchmark(): Unit = {
    val bsrInput: Vector[Vector[(Int, Vector[Vector[Float]])]] = generateBsr(10, 10, 100, 2, 10)
    val denseInput: Vector[Vector[Float]] = denseFromBsr(100, bsrInput)
    val csrInput = csrFromDense(denseInput)

    val (denseTime, denseRes) = profile(Programs.denseMV.evaluate(denseInput))
    val (csrTime, csrRes) = profile(Programs.csrMV.evaluate(csrInput))
    val (bsrTime, bsrRes) = profile(Programs.bsrMV.evaluate(bsrInput))

    println("Helloo")

    println(s"dense ${denseRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${denseTime / 1e+9}")
    println(s"csr ${csrRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${csrTime / 1e+9}")
    println(s"bsr ${bsrRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${bsrTime / 1e+9}")
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










