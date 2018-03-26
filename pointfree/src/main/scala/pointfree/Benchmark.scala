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

  def median(s: Seq[Long]): Long = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
  }

  @Test
  def csrBenchmark(): Unit = {
    val reps = 30
    val bench = List(1, 2, 4, 8, 16).map(density => {
      val denseInput: Vector[Vector[Float]] = generateDense(1000, 1000, density)
      val csrInput: Vector[Vector[(Int, Float)]] = csrFromDense(denseInput)
      val denseTime = median((0 to reps).map(_ => profile(Programs.denseMV.evaluate(denseInput))._1))
      val csrTime = median((0 to reps).map(_ => profile(Programs.csrMV.evaluate(csrInput))._1))
      (denseTime, csrTime)
    })
    bench.unzip match {
      case (dense, csr) =>
        println(dense)
        println(csr)
    }
  }

  @Test
  def bsrBenchmark(): Unit = {
    val bsrInput: Vector[Vector[(Int, Vector[Vector[Float]])]] = generateBsr(10, 10, 100, 2, 10)
    val denseInput: Vector[Vector[Float]] = denseFromBsr(100, bsrInput)
    val csrInput = csrFromDense(denseInput)

    val (denseTime, denseRes) = profile(Programs.denseMV.evaluate(denseInput))
    val (csrTime, csrRes) = profile(Programs.csrMV.evaluate(csrInput))
    val (bsrTime, bsrRes) = profile(Programs.bsrMV.evaluate(bsrInput))

    println(s"dense ${denseRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${denseTime / 1e+9}")
    println(s"csr ${csrRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${csrTime / 1e+9}")
    println(s"bsr ${bsrRes.unwrap.asInstanceOf[Vector[Float]].take(10)} ${bsrTime / 1e+9}")
  }

  @Test
  def mssBenchmark(): Unit = {
    val spec = Programs.maxSegSum
    val opt = Reduce(Max) *: Scan(Plus *: Max(Zero))(Zero)
    val input = List.fill(500)(util.Random.nextFloat())
    val (specTime, specResult) = profile(spec.evaluate(input).unwrap.asInstanceOf[Float])
    val (optTime, optResult) = profile(opt.evaluate(input).unwrap.asInstanceOf[Float])
    println(s"$specResult ${specTime / 1e+9}")
    println(s"$optResult ${optTime / 1e+9}")
  }
}
