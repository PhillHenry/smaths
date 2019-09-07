package uk.co.odinconsultants.smaths.functions

import uk.co.odinconsultants.smaths.functions.Discrete.gamma

object Continuous {

  def beta[T: Integral](x: T, y: T): Option[Double] = {
    val op = implicitly[Numeric[T]]
    import op._
    val gxy = for (gx <- gamma(x);
                   gy <- gamma(y)) yield times(gx, gy)
    for (xy <- gxy;
         gXPlusY <- gamma(plus(x, y)) ) yield toDouble(xy) / toDouble(gXPlusY)
  }

}
