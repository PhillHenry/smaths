package uk.co.odinconsultants.smaths.functions

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DiscreteSpec extends WordSpec with Matchers {

  import Discrete._

  "Gamma(6)" should {
    "equal 5!" in {
      gamma(6) shouldBe Some(120)
    }
  }

}
