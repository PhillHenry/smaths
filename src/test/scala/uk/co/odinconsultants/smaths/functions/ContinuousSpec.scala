package uk.co.odinconsultants.smaths.functions

import org.scalatest.{Matchers, WordSpec}

class ContinuousSpec extends WordSpec with Matchers {

  import Continuous._

  "Beta(4, 2)" should {
    "be (6 * 1) / 120" in {
      beta(4, 2) shouldBe Some(1d/20)
    }
  }

}
