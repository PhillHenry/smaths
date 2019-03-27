package uk.co.odinconsultants.smaths.stats

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ShannonEntropySpec extends WordSpec with Matchers {

  import ShannonEntropy._

  "Entropy of p=0.5" should {
    "be 2" in {
      entropyOf(0.5) shouldBe 0.5
    }
  }

}
