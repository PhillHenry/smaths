package uk.co.odinconsultants.smaths.stats

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DivergenceSpec extends WordSpec with Matchers {

  import Divergence._

  "Kullback-Leibler" should {
    "be calculated" in new KullbackLeiblerFixture {
      qs.zip(ps).map { case (q, p) => kl(q, p) }.sum shouldBe KL_Q_P +- tolerance
    }
  }

}
