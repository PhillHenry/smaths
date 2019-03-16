package uk.co.odinconsultants.smaths.stats

import uk.co.odinconsultants.smaths.linalg.Tensors._

object Divergence {

  type CompareFn = (Double, Double) => Double

  val LaplaceSmoothingKL: CompareFn = { case (x, y) => kl(x + 1, y + 1) }
  val ZerosIgnoredKL:     CompareFn = { case (x, y) => if (x == 0d || y == 0d) 0 else kl(x, y) }
  val Average:            CompareFn = { case (x, y) => (x + y) / 2 }

  /**
    * @see https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
    */
  def kl(p: Double, q: Double): Double = p * math.log(p / q)

  def on(fn: CompareFn, ps: Vec[Double], qs: Vec[Double]): Vec[Double] =
    ps.zip(qs).map { case(x, y) => fn(x, y) }

  def total(fn: CompareFn, ps: Vec[Double], qs: Vec[Double]): Double =
    on(fn, ps, qs).sum

  /**
    * @see https://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence
    */
  def jensenShannon(ps: Vec[Double], qs: Vec[Double]): Double = {
    val ms  = on(Average, ps, qs)
    val dpm = total(ZerosIgnoredKL, ps, ms)
    val dqm = total(ZerosIgnoredKL, qs, ms)
    (dpm + dqm) / 2
  }

}
