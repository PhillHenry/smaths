package uk.co.odinconsultants.smaths.histogram

import scala.math.Ordering

object Buckets {

  type LeftIndices[T] = (Seq[T], T)

  type MaybeBuckets[T] = Option[LeftIndices[T]]

  def diff[T: Numeric](xs: List[T]): Seq[T] = {
    val op = implicitly[Numeric[T]]
    xs match {
      case x :: y :: _ =>
        val d = op.minus(y, x)
        Seq(d)
      case _ => Seq()
    }
  }

  /**
    * @see https://en.wikipedia.org/wiki/Interquartile_range
    */
  def interQuartiles[T: Ordering](xs: Seq[T]): Option[(T, T)] = {
    if (xs.size < 4)
      None
    else {
      val sorted  = xs.sorted.toList
      val n       = xs.size
      val low     = math.floor(n * 0.25).toInt
      val high    = math.ceil(n * 0.75).toInt - 1
      println(s"low = $low, high = $high")
      Some((sorted(low), sorted(high)))
    }
  }

  def iqr[T: Numeric](xs: Seq[T]): Option[T] = {
    val op = implicitly[Numeric[T]]
    interQuartiles(xs).map { case (low, high) => op.minus(high, low) }
  }

  /**
    * @see https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
    */
  def freedmanDiaconis[T: Numeric](xs: Seq[T]): Option[Double] = {
    val op = implicitly[Numeric[T]]
    iqr(xs).map { i =>
      (2 * op.toDouble(i)) / math.pow(xs.size, 1.0 / 3.0)
    }
  }

  def widths[T: Numeric](xs: Seq[T]): Seq[T] = {
    xs.sliding(2).flatMap(x => diff(x.toList)).toSeq
  }

  def bucketInclMins[T: Ordering](xs: Set[T], nBuckets: Int): MaybeBuckets[T] =
    if (xs.isEmpty)
      None
    else {
      val chunk     = math.floor(xs.size.toDouble / nBuckets).toInt
      val chunked   = xs.toList.sorted.grouped(if (chunk == 0) 1 else chunk).map(_.head).toSeq
      Some((chunked.take(nBuckets), xs.max))
    }

  def inRangeInclExcl[T: Ordering](t: T, x: T, y: T): Boolean = {
    val op = implicitly[Ordering[T]]
    op.gteq(t, x) && op.lt(t, y)
  }

  def inRangeInclIncl[T: Ordering](t: T, x: T, y: T): Boolean = {
    val op = implicitly[Ordering[T]]
    op.gteq(t, x) && op.lteq(t, y)
  }

  def findMinBucketValueFor[T: Ordering](t: T, ts: Seq[T]): Option[T] = ts.sliding(2).find { xs =>
    val x = xs.head
    val y = xs.last
    inRangeInclExcl(t, x, y)
  }.map(_.head)

  def findFn[T: Ordering](indices: LeftIndices[T])(t: T): Option[T] = {
    val (leftMost, lastInRange) = indices
    findMinBucketValueFor(t, leftMost) match {
      case y @ Some(_)  => y
      case None         =>
        if (inRangeInclIncl(t, leftMost.last, lastInRange))
          Some(leftMost.last)
        else
          None
    }
  }

  def find[T: Ordering](t: T, buckets: MaybeBuckets[T]): Option[T] =
    buckets.flatMap { case indices =>
      findFn(indices)(t)
    }

}
