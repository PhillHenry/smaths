package uk.co.odinconsultants.smaths.functions

import org.scalatest.{Matchers, WordSpec}

class DiscreteSpec extends WordSpec with Matchers {

  import Discrete._

  "Gamma(6)" should {
    "equal 5!" in {
      gamma(6) shouldBe Some(120)
    }
  }

}
