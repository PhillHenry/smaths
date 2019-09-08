package uk.co.odinconsultants.smaths.arithmetic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.Result.errorIn

@RunWith(classOf[JUnitRunner])
class CombinatoricsSpec extends WordSpec with Matchers {

  import Combinatorics._

  "C(7, 3)" should {
    "be 35" in {
      val result = binomialCoefficiet(7, 3)
      errorIn(result) shouldBe false
      result foreach { _  shouldBe 35 }
    }
  }

}
