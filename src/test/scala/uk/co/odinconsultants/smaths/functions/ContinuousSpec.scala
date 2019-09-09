package uk.co.odinconsultants.smaths.functions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.Result.errorIn

@RunWith(classOf[JUnitRunner])
class ContinuousSpec extends WordSpec with Matchers {

  import Continuous._

  "Beta(4, 2)" should {
    "be (6 * 1) / 120" in {
      val result = B(4, 2)
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 1d/20 }
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
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 8.27150 +- 0.00001 }
    }
  }

}
