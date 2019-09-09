package uk.co.odinconsultants.smaths.functions

import uk.co.odinconsultants.smaths.functions.Discrete.gamma

object Continuous {

  def B[T: Integral](x: T, y: T): Option[Double] = {
    val op = implicitly[Numeric[T]]
    import op._
    val gxy = for (gx <- gamma(x);
                   gy <- gamma(y)) yield times(gx, gy)
    for (xy <- gxy;
         gXPlusY <- gamma(plus(x, y)) ) yield toDouble(xy) / toDouble(gXPlusY)
  }

  def modeBeta[T: Integral](x: T, y: T): Double = {
    val op = implicitly[Numeric[T]]
    val a = op.toDouble(x)
    val b = op.toDouble(y)
    (a - 1) / (a + b - 2)
  }

  def pdfBeta[T: Integral](x: Double, a: T, b: T): Option[Double] = {
    val op = implicitly[Numeric[T]]
    import op._
    B(a, b).map { d =>
      val alpha = toDouble(a)
      val beta = toDouble(b)
      println(s"d = $d")
      math.pow(x, alpha - 1) * math.pow(1 - x, beta - 1) / d
    }
  }

}
