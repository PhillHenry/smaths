package uk.co.odinconsultants.smaths.histogram

import uk.co.odinconsultants.smaths.histogram.Buckets.{MaybeBuckets, bucketInclMins}

import scala.util.Random

trait UniformRandomFixture {
  val rand: Random              = new Random(1)
  val xs: Seq[Double]           = (1 to 101).map(_ => rand.nextDouble() * 10)
  val k                         = 11
  val buckets: MaybeBuckets[Double]  = bucketInclMins(xs.toSet, k)
}
