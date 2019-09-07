package uk.co.odinconsultants.smaths.functions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ContinuousSpec extends WordSpec with Matchers {

  import Continuous._

  "Beta(4, 2)" should {
    "be (6 * 1) / 120" in {
      beta(4, 2) shouldBe Some(1d/20)
    }
  }

  "Mode of a beta(65, 42)" should {
    "be approx 0.6095" in {
      modeBeta(65, 42) shouldBe 0.6095 +- 0.00009
    }
  }

  "PDF of a beta binomial(65, 42)" should {
    "be approx 8.271509547781616 at x=0.6" in {
      val result = pdfBeta(0.6, BigInt(65), BigInt(42))
      result should not be None
      result.get shouldBe 8.27150 +- 0.00001
    }
  }

}
