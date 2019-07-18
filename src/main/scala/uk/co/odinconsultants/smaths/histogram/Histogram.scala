package uk.co.odinconsultants.smaths.histogram

object Histogram {

  type Histogram[T, U]  = Map[T, U]

  type Probabilities[T] = Map[T, Double]

  import Buckets.MaybeBuckets

  type NormalizeFn[T, U] = (T, T) => U

  def identityT[T: Numeric](t: T, u: T): T = t

  def counts[T: Numeric](ts: Seq[T], buckets: MaybeBuckets[T]): Histogram[T, T] = {
    val op = implicitly[Numeric[T]]
    val hs = collection.mutable.Map[T, T]() withDefault (_ => op.zero)
    ts.foreach { t =>
      Buckets.find(t, buckets).foreach { i =>
        val old = hs(i)
        hs(i)   = op.plus(old, op.one)
      }
    }
    hs.toMap
  }

  def normalize[T, U: Numeric](t2c: Histogram[T, U]): Probabilities[T] = {
    val op    = implicitly[Numeric[U]]
    val total = op.toDouble(t2c.values.sum)
    t2c.mapValues(x => op.toDouble(x) / total)
  }

  def normFn[T: Numeric](i2d: Map[T, T]): NormalizeFn[T, Double] = { case (i, x) =>
    val opT = implicitly[Numeric[T]]
    val v: Double = opT.toDouble(x)
    val d: Double = opT.toDouble(i2d(i))
    v / d
  }

  def normalizedDistribution[T: Numeric](ts: Seq[T], buckets: MaybeBuckets[T]): Probabilities[T] = buckets.map { case (xs, last) =>
    val opT   = implicitly[Numeric[T]]
    val ds    = Buckets.widths(xs :+ last)
    val i2d   = xs.zip(ds).toMap
    counts(ts, buckets).map { case (k, v) =>
        val d = i2d(k)
        k -> (opT.toDouble(v) / opT.toDouble(d))
    }
  }.getOrElse(Map.empty)


}
