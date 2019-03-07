package uk.co.odinconsultants.smaths.stats

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import collection.immutable.Seq

@RunWith(classOf[JUnitRunner])
class AveragesSpec extends WordSpec with Matchers {

  import Averages._

  /**
    * See https://en.wikipedia.org/wiki/Bessel%27s_correction
    */
  val xs: Seq[Int] = Seq(2051, 2053, 2055, 2050, 2051)

  s"No Bessel's correction of ${xs.mkString}" should {
    "lead to Wikipedia's mean" in {
      mean(xs) shouldBe 2052
    }
    "lead to Wikipedia's variance" in {
      variance(xs, bessel = false) shouldBe 3.2
    }
  }

  s"Bessel's correction of ${xs.mkString}" should {
    "lead to Wikipedia's variance" in {
      variance(xs, bessel = true) shouldBe 4d
    }
    "lead to a standard deviation" in {
      stdDev(xs, bessel = true) shouldBe 2d
    }
  }


}
