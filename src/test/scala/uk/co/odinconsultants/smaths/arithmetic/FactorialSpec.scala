package uk.co.odinconsultants.smaths.arithmetic

import org.scalatest.{Matchers, WordSpec}

class FactorialSpec extends WordSpec with Matchers {

  import Factorial._

  "5!" should {
    "be 120" in {
      factorial(5) shouldBe 120
    }
  }

}
