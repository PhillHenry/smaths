package uk.co.odinconsultants.smaths.histogram

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class HistogramSpec extends WordSpec with Matchers {

  import Histogram._

  "Histogram with k < n" should {
    val xs      = 1 to 101
    val k       = 11
    val buckets = Buckets.bucketInclMins(xs.toSet, k)
    "total values equal to n" in {
      val hs = counts(xs, buckets)
      withClue(s"\n${hs.toArray.sortBy(_._1).mkString("\n")}\nbuckets = $buckets\n") {
        hs should have size k
        hs.values.sum shouldBe xs.size
      }
    }
  }

  "Normalized histograms" should {
    "total to 1.0" in new UniformRandomFixture {
      normalize(counts(xs, buckets)).values.sum shouldBe 1.0 +- 0.00001
    }
  }

}
