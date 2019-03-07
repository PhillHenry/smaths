package uk.co.odinconsultants.smaths.linalg

import org.scalatest.{Matchers, WordSpec}

class TensorsSpec extends WordSpec with Matchers {

  import Tensors._

  "Adding vectors" should {
    "add elements bitwise" in {
      add(Array(1, 2, 3), Array(3, 2, 1)) shouldBe Array(4, 4, 4)
    }
    "should allow different Numeric types" ignore {
      //add(Array(1d, 2d, 3d), Array(3, 2, 1)) shouldBe Array(4d, 4d, 4d)
    }
  }

}
