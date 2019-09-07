package uk.co.odinconsultants.smaths.arithmetic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class FactorialSpec extends WordSpec with Matchers {

  import Factorial._

  "5!" should {
    "be 120" in {
      factorial(5) shouldBe Some(120)
    }
    "be 120 for BigInt too" in {
      factorial(BigInt(5)) shouldBe Some(BigInt(120))
    }
  }

  "0" should {
    "be 1" in {
      factorial(0) shouldBe Some(1)
    }
  }

  "Negative numbers" should {
    "be undefined" in {
      factorial(-1) shouldBe None
    }
  }

}
