package uk.co.odinconsultants.smaths.stats

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import uk.co.odinconsultants.smaths.linalg.Tensors._

@RunWith(classOf[JUnitRunner])
class ChiSquareSpec extends WordSpec with Matchers {

  import ChiSquare._

  /**
    * see https://www.spss-tutorials.com/chi-square-independence-test/
    */
  val spreadSheet: String = """18    36    21    9     6
                              |12    36    45    36    21
                              |6     9     9     3     3
                              |3     9     9     6     3""".stripMargin

  val data: Matrix[Int] = spreadSheet.split("\n").map(_.split(" ").filterNot(_ == "").map(_.toInt))

  "Column means" should {
    "be as defined in the example" in {
      val means = columnMeans(data)
      means(0) shouldBe 9.75
      means(1) shouldBe 22.5
      means(2) shouldBe 21d
      means(3) shouldBe 13.5d
      means(4) shouldBe 8.25
    }
  }

  "Chi Square" should {
    "be 23.57 for the first row according to the examples website" in {
      println(data.map(_.mkString("\t")).mkString("\n")) // TODO
    }
  }

}
