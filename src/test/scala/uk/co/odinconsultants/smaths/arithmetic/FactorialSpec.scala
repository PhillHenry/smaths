package uk.co.odinconsultants.smaths.arithmetic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.Result.errorIn

@RunWith(classOf[JUnitRunner])
class FactorialSpec extends WordSpec with Matchers {

  import Factorial._

  "5!" should {
    "be 120" in {
      val result = factorial(5)
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 120 }
    }
    "be 120 for BigInt too" in {
      factorial(BigInt(5)) shouldBe Some(BigInt(120))
    }
  }

  "0" should {
    "be 1" in {
      val result = factorial(0)
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 1 }
    }
  }

  "Negative numbers" should {
    "be undefined" in {
      val result = factorial(-1)
      errorIn(result) shouldBe true
    }
  }

}
