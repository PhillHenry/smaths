package uk.co.odinconsultants.smaths.stats

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DivergenceSpec extends WordSpec with Matchers {

  import Divergence._

  "Kullback-Leibler" should {
    "be calculated" in new KullbackLeiblerFixture {
      total(ZerosIgnoredKL, qs, ps) shouldBe KL_Q_P +- tolerance
    }
  }

  "Jensen-Shannon" should {
    "tolerate zeros" in new KullbackLeiblerFixture {
      jensenShannon(ps :+ 0d, qs :+ 0d) shouldBe jensenShannon(ps, qs)
    }
    "be symmetric" in new KullbackLeiblerFixture {
      jensenShannon(ps, qs) shouldBe jensenShannon(qs, ps)
    }
  }

}
