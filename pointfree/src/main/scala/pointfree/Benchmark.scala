package pointfree

import org.junit.Test

import scalaz._
import Scalaz._
import scala.collection.mutable.ListBuffer
import scala.util.Random

class Benchmark {
  def generateDense(m: Int, n: Int, density: Int): Vector[Vector[Float]] =
    Vector.fill(m)(Vector.fill(m)(if (util.Random.nextInt(density) == 0) util.Random.nextFloat() else 0))

  def csrFromDense(dense: Vector[Vector[Float]]): Vector[Vector[(Int, Float)]] =
    dense.map(row => row
      .zipWithIndex
      .filter({case (v, _) => v.abs > 1e-6 })
      .map(_.swap))

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

  def median(s: Seq[Long]): Long = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
  }

  val scalaVector: Array[Float] = Array.fill(1000)(Random.nextFloat())

  def scalaDenseMV(matrix: Vector[Vector[Float]], vector: Array[Float]): Vector[Float] =
    matrix.map(row => row.zip(vector).foldl(0.asInstanceOf[Float])(acc => {case (a, b) => acc + a * b}))

  def scalaCsrMV(matrix: Vector[Vector[(Int, Float)]], vector: Array[Float]): Vector[Float] =
    matrix.map(row => row.foldl(0.asInstanceOf[Float])(acc => {case (i, v) => acc + vector(i) * v}))

  @Test
  def csrBenchmark(): Unit = {
    val reps = 10
    val bench = Vector(1, 2, 4, 8, 16).map(density => {
      val denseInput: Vector[Vector[Float]] = generateDense(1000, 1000, density)
      val csrInput: Vector[Vector[(Int, Float)]] = csrFromDense(denseInput)

      val denseTime = median((0 until reps).map(_ => profile(Programs.denseMV.evaluate(denseInput))._1))
      val csrTime = median((0 until reps).map(_ => profile(Programs.csrMV.evaluate(csrInput))._1))
      val scalaDenseTime = median((0 until reps).map(_ => profile(scalaDenseMV(denseInput, scalaVector))._1))
      val scalaCsrTime = median((0 until reps).map(_ => profile(scalaCsrMV(csrInput, scalaVector))._1))

      Vector(denseTime, csrTime, scalaDenseTime, scalaCsrTime)
    })
    println(bench.transpose.map(_.map(_ / 1e+9)).map(_.mkString(" ")).mkString("\n"))
  }

  @Test
  def bsrBenchmark(): Unit = {
    val reps = 10
    val bench = Vector(1, 2, 4).map(blockDensity => {
      val bsrInput: Vector[Vector[(Int, Vector[Vector[Float]])]] = generateBsr(10, 10, 100, blockDensity, 1)
      val denseInput: Vector[Vector[Float]] = denseFromBsr(100, bsrInput)
      val csrInput = csrFromDense(denseInput)

      val denseTime = median((0 until reps).map(_ => profile(Programs.denseMV.evaluate(denseInput))._1))
      val csrTime = median((0 until reps).map(_ => profile(Programs.csrMV.evaluate(csrInput))._1))
      val bsrTime = median((0 until reps).map(_ => profile(Programs.bsrMV.evaluate(bsrInput))._1))

      Vector(denseTime, csrTime, bsrTime)
    })
    println(bench.transpose.map(_.map(_ / 1e+9)).map(_.mkString(" ")).mkString("\n"))
  }

  @Test
  def mssBenchmark(): Unit = {
    val spec = Programs.maxSegSum
    val opt = Reduce(Max) *: Scan(Plus *: Max(Zero))(Zero)
    val input = Vector.fill(500)(util.Random.nextFloat())
    val (specTime, specResult) = profile(spec.evaluate(input).unwrap.asInstanceOf[Float])
    val (optTime, optResult) = profile(opt.evaluate(input).unwrap.asInstanceOf[Float])
    println(s"$specResult ${specTime / 1e+9}")
    println(s"$optResult ${optTime / 1e+9}")
  }

  @Test
  def mssDebug(): Unit = {
    val segs: Expr = Join *: Map(Tails) *: Inits
    val maxSegSum: Expr = Reduce(Max) *: Map(Fold(Plus)(Zero)) *: segs
  }
}
