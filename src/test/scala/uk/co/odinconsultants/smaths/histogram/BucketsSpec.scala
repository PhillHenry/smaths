package uk.co.odinconsultants.smaths.histogram

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class BucketsSpec extends WordSpec with Matchers {

  import Buckets._

  "Freedman-Diaconis" should {
    "agree with example at http://www.jtrive.com/determining-histogram-bin-width-using-the-freedman-diaconis-rule.html" in {
      val xs = Seq(62.55976, -14.71019, -20.67025, -35.43758, -10.65457,  21.55292,
        41.26359,   0.33537, -14.43599, -40.66612,   6.45701, -40.39694,
        55.1221,  24.50901,   6.61822, -29.10305,   6.21494,  15.25862,
        13.54446,   2.48212,  -2.34573, -21.47846,   -5.0777,  26.48881,
        -8.68764,  -5.49631,  42.58039,  -6.59111, -23.08169,  19.09755,
        -21.35046,   0.24064,  -3.16365, -37.43091,  24.48556,    2.6263,
        31.14471,   5.75287,  -46.8529, -14.26814,   8.41045,  18.11071,
        -30.46438,  12.22195, -31.83203,  -8.09629,  52.06456, -24.30986,
        -25.62359,   2.86882,  15.77073,  31.17838, -22.04998)
      val maybe = freedmanDiaconis(xs)
      maybe should not be None
      maybe foreach { result => result shouldBe 19.76483815603517 +- 0.0001 }
    }
  }

  "IQR from Wikipedia" should {
    "agree with example" in {
      iqr(Seq(7, 7, 31, 31, 47, 75, 87, 115, 116, 119, 119, 155, 177)) shouldBe Some(88)
    }
  }

  "Inter-quartiles" should {
    "not be calculated if there are fewer than 4 elements (or else how can you do it?)" in {
      interQuartiles(Seq(3)) shouldBe None
    }
    "be the second and third elements in a 4-element list" in {
      interQuartiles(Seq(1,2,3,4)) shouldBe Some((2, 3))
    }
    "be 25 and 75 in 100 elements" in {
      interQuartiles(0 to 100) shouldBe Some((25, 75))
    }
  }

  val nBuckets = 11

  "An empty collection" should {
    "be handled" in {
      bucketInclMins(Set.empty[Double], nBuckets)
    }
  }

  "Gaps between fibonacci" should {
    "be previous number" in {
      lazy val fibs: Stream[Int] = 0 #:: 1 #:: (fibs zip fibs.tail).map{ case (x, y) => x + y }
      val fs = fibs.slice(1, 10).toList
      withClue(s"sequence: $fs\n") {
        widths(fs) shouldBe (0 +: fs.take(fs.size - 2))
      }
    }
  }

  s"$nBuckets buckets" should {
    val each  = 7
    val range = 1 to (nBuckets * each)

    def fullRange(range: Range = range): CheckBucketFn[Int, Unit] = { case (indices, max) =>
      indices should have size nBuckets
      max shouldBe range.max
    }

    val boundsChecking: CheckBucketFn[Int, Unit] = {  case (indices, max) =>
      withClue(s"${indices.mkString(",")}\n") {
        indices.min shouldBe range.min
        indices.max shouldBe (range.max - each + 1)
      }
    }
    val spacingFn: CheckBucketFn[Int, Unit] = {  case (indices, max) =>
      val spaces = widths(indices)
      spaces.toSet shouldBe Set(each)
    }

    s"evenly distribute over $range" in {
      Set(boundsChecking, fullRange(), spacingFn) foreach { fn =>
        checkBucketsFor(range.toSet, nBuckets, fn)
      }
    }

    val badFitRange = 1 to (nBuckets * each + 1)
    s"almost distribute evenly over $badFitRange" in {
      checkBucketsFor(badFitRange.toSet, nBuckets, fullRange(badFitRange))
    }

    s"have ${nBuckets - 1} empty when populated with one element" in {
      val element = 1
      val fn: CheckBucketFn[Int, Unit] = { case (indices, max) =>
        indices should have size 1
        max shouldBe element
      }
      checkBucketsFor(Set(element), nBuckets, fn)
    }
  }

  "Unevenly fitting elements" should {
    val xs      = 1 to 101
    val k       = 11
    s"still produce $k buckets from $xs" in {
      val fn: CheckBucketFn[Int, Unit] = {  case (indices, max) =>
        indices should have size k
      }
      checkBucketsFor(xs.toSet, k, fn)
    }
  }

  type CheckBucketFn[T, U] = ((Seq[T], T)) => U
  def checkBucketsFor[T: Ordering, U](xs: Set[T], k: Int, fn: CheckBucketFn[T, U]): Unit = {
    val buckets = bucketInclMins(xs, k)
    buckets should not be None
    buckets foreach fn
  }

  "Ranges" should {
    val x = 1
    val y = 7
    "be inclusive on first tuple" in {
      inRangeInclExcl(1, x, y) shouldBe true
    }
    "be exclusive on second tuple" in {
      inRangeInclExcl(y, x, y) shouldBe false
    }
    "be include [x, y)" in {
      inRangeInclExcl(x + 1, x, y) shouldBe true
    }
  }

  "Searching for a value" should {
    val each    = 7
    val min     = 1
    val range   = min until (nBuckets * each, each)
    val max     = range.max + each - 1
    val buckets = Some((range, max))
    "return left-most value of bucket when in-range" in {
      withClue(s"${range.mkString(", ")}, $max\n") {
        find(1, buckets).get shouldBe 1
        find(2, buckets).get shouldBe 1
        find(max, buckets).get shouldBe range.last
      }
    }
    "return nothing if out of range" in {
      find(min - 1, buckets) shouldBe None
      find(max + 1, buckets) shouldBe None
    }
  }

}
