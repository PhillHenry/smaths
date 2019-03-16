package uk.co.odinconsultants.smaths.linalg

import org.scalatest.{Matchers, WordSpec}

class TensorsSpec extends WordSpec with Matchers {

  import Tensors._

  val v1:       Vec[Int]  = toVec(1, 2, 3)
  val v2:       Vec[Int]  = toVec(3, 2, 1)
  val v1PlusV2: Vec[Int]  = toVec(4, 4, 4)

  "Adding rows" should {
    "combine all rows" in {
      addRows(toVec(v1, v2)) shouldBe v1PlusV2
    }
  }

  "Adding vectors" should {
    "add elements bitwise" in {
      add(v1, v2) shouldBe v1PlusV2
    }
    "should allow different Numeric types" ignore {
//      add(Array(1d, 2d, 3d), Array(3, 2, 1)) shouldBe Array(4d, 4d, 4d)
    }
  }

  "L2 normalization" should {
    "produce elements that when squared add to 1.0" in {
      l2Normalize(v1).map(x => x * x).sum shouldBe 1.0 +- 0.00001
    }
  }

  "L1 normalization" should {
    "produce elements that add to 1.0" in {
      l1Normalize(v1).sum shouldBe 1.0 +- 0.00001
    }
  }

}
