package uk.co.odinconsultants.smaths.arithmetic

import uk.co.odinconsultants.smaths.arithmetic.Factorial.factorial

object Combinatorics {

  def binomialCoefficiet[T: Integral](n: T, k: T): Option[Double] = {
    val op = implicitly[Numeric[T]]
    import op._
    for (x <- factorial(n);
         y <- factorial(k);
         z <- factorial(minus(n, k))) yield toDouble(x) / (toDouble(y) * toDouble(z))
  }

}
