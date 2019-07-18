package uk.co.odinconsultants.smaths.histogram

import uk.co.odinconsultants.smaths.histogram.Buckets.{MaybeBuckets, find}
import uk.co.odinconsultants.smaths.histogram.Histogram._

/**
  * @see https://github.com/yzhao062/pyod/blob/master/pyod/models/hbos.py
  * @see https://github.com/Kanatoko/HBOS-python/blob/master/hbos.py
  * @see https://github.com/Markus-Go/rapidminer-anomalydetection/tree/21971947ec6d6c7cfc34c1767581d43c6daada77
  */
object Scoring {

  type Score = Double

  def score[T: Numeric](t: T, ts: Seq[T], buckets: MaybeBuckets[T], alpha: Double = 0): Score = {
    val hs = normalizedDistribution(ts, buckets)
    val ps = normalize(hs)
    val p  = find(t, buckets).map(ps(_)).getOrElse(0d)
    math.log(p + alpha)
  }

}
