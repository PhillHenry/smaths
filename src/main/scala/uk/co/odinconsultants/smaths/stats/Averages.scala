package uk.co.odinconsultants.smaths.stats

import math.pow

object Averages {

  def mean[T : Numeric](xs: Seq[T]): Double =
    implicitly[Numeric[T]].toDouble(xs.sum) / xs.size

  def variance[T : Numeric](xs: Seq[T], bessel: Boolean): Double = {
    val mu  = mean(xs)
    val n   = if (bessel) xs.size - 1 else xs.size
    xs.map { x => pow(implicitly[Numeric[T]].toDouble(x) - mu, 2) }.sum / n
  }

  def stdDev[T : Numeric](xs: Seq[T], bessel: Boolean): Double =  pow(variance(xs, bessel), 0.5)

}
