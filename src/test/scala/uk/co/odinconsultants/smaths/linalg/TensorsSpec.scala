package uk.co.odinconsultants.smaths.linalg

import org.scalatest.{Matchers, WordSpec}

class TensorsSpec extends WordSpec with Matchers {

  import Tensors._

  "Adding vectors" should {
    "add elements bitwise" in {
      add(Array(1, 2, 3), Array(3, 2, 1)) shouldBe Array(4, 4, 4)
    }
    "should allow different Numeric types" in {
//      add(Array(1d, 2d, 3d), Array(3, 2, 1)) shouldBe Array(4d, 4d, 4d)
    }
  }

  "L2 normalization" should {
    val xs = Array(1, 2, 3, 4)
    "produce elements that when squared add to 1.0" in {
      l2Normalize(xs).map(x => x * x).sum shouldBe 1.0 +- 0.00001
    }
  }

  "L1 normalization" should {
    val xs = Array(1, 2, 3, 4)
    "produce elements that add to 1.0" in {
      l1Normalize(xs).sum shouldBe 1.0 +- 0.00001
    }
  }

}
