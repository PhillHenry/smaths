package uk.co.odinconsultants.smaths.arithmetic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class CombinatoricsSpec extends WordSpec with Matchers {

  import Combinatorics._

  "C(7, 3)" should {
    "be 35" in {
      binomialCoefficiet(7, 3) shouldBe Some(35)
    }
  }

}
