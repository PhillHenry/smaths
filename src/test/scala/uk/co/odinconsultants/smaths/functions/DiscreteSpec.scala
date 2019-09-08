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

}
