package uk.co.odinconsultants.smaths.histogram

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.stats.Averages._
import uk.co.odinconsultants.smaths.histogram.Buckets._

import scala.Double.NegativeInfinity

@RunWith(classOf[JUnitRunner])
class ScoringSpec extends WordSpec with Matchers {

  import Scoring._

  "A totally new event" should {
    type T = Int
    val noInfo: Seq[T]       = Seq[T]()
    val buckets: MaybeBuckets[T]  = bucketInclMins(noInfo.toSet, 10)
    val alpha                     = 0.1

    "have a score based on alpha" in {
      score(1, noInfo, buckets, alpha) shouldBe math.log(alpha)
    }

    "have a negative infinity score if alpha=0" in {
      score(1, noInfo, buckets) shouldBe NegativeInfinity
    }
  }

  "A histogram of random numbers" should {

    "yield a score above alpha for an arbitrary value already seen" in new UniformRandomFixture {
      score(xs.head, xs, buckets) shouldBe > (NegativeInfinity)
      score(xs.last, xs, buckets) shouldBe > (NegativeInfinity)
    }
    "yield negative infinity for unknown value (alpha=0)" in new UniformRandomFixture {
      score(xs.min - 1, xs, buckets) shouldBe NegativeInfinity
      score(xs.max + 1, xs, buckets) shouldBe NegativeInfinity
    }
  }

  "Different data" should {
    "yield different scores" in new RandomDataFixture {
      val x:              Seq[Score]  = randomData(10)
      val y:              Seq[Score]  = randomData(10)
      scoreArbitraryValue(x) should not be equal (scoreArbitraryValue(y))
    }
  }

  "Outliers" should {
    "have lower score than the median" in new RandomDataFixture {
      val n               = 101
      val xs: Seq[Double] = randomData(n)
      withClue(s"\nmin = ${xs.min}\nmedian = ${medianVal(xs)}\n") {
        val randomScore = scoreMin(xs)
        val medianScore = toScore(medianVal)(xs)
        println(s"score on random data = $randomScore, score on median data = $medianScore")
        randomScore should be < medianScore
      }
    }
  }

  "The average score of outliers" should {
    "be less than the average of random points" in new RandomDataFixture {
      val n                           = 100
      val clean:    Seq[Seq[Double]]  = (1 to n).map(_ => randomData(nBuckets * 10))
      val avRandom: Double            = mean(clean.map(scoreArbitraryValue))
      val avMin:    Double            = mean(clean.map(scoreMin))

      withClue(s"\nrandom scores = ${clean.map(scoreArbitraryValue).mkString(", ")}\nscores of min elements = ${clean.map(scoreMin).mkString(", ")}\n"){
        println(s"score on random data = $avRandom, score on min data = $avMin")
        avRandom shouldBe > (avMin)
      }
    }
  }

}
