package uk.co.odinconsultants.smaths.arithmetic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class LogsSpec extends WordSpec with Matchers {

  import Logs._

  "16" should {
    "be 4 in base 2" in {
      log(2, 16) shouldBe 4
      logBase2(16) shouldBe 4
    }
  }

}
