package uk.co.odinconsultants.smaths.functions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.Result.errorIn

@RunWith(classOf[JUnitRunner])
class DiscreteSpec extends WordSpec with Matchers {

  import Discrete._

  "Gamma(6)" should {
    "equal 5!" in {
      val result = gamma(6)
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 120 }
    }
  }

  "Closed form Gamma" should {
    "equal square root of gamma for 0.5" in {
      val actual: Option[Double] = gammaOfHalfPlus(0)
      actual should not be None
      actual.get shouldBe SqrtPI +- 0.0001
    }
  }

}
