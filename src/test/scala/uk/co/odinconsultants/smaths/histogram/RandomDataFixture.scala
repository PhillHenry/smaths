package uk.co.odinconsultants.smaths.histogram

import uk.co.odinconsultants.smaths.histogram.Buckets.bucketInclMins
import uk.co.odinconsultants.smaths.histogram.Scoring.{Score, score}

import scala.util.Random

trait RandomDataFixture {
  val nBuckets  = 10
  val rand      = new Random(1)

  type ChooseValue = Seq[Double] => Double

  def randomData(k: Int): Seq[Double] = (1 to k).map(_ => rand.nextGaussian() * 10)

  def toScore(fn: ChooseValue)(xs: Seq[Double]): Score = {
    val buckets = bucketInclMins(xs.toSet, nBuckets)
    val x       = fn(xs)
    score(x, xs, buckets)
  }

  val medianVal: ChooseValue = { xs =>
    xs.sorted.drop(xs.size / 2).head
  }

  val scoreArbitraryValue:  Seq[Double] => Score  = toScore(_.head) _
  val scoreMin:             Seq[Double] => Score  = toScore(_.min) _
}
