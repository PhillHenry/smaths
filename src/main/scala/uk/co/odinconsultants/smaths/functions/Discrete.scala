package uk.co.odinconsultants.smaths.functions

import uk.co.odinconsultants.smaths.Result.IntegralResult
import uk.co.odinconsultants.smaths.arithmetic.Factorial.factorial

object Discrete {

  def gamma[T: Integral](x: T): IntegralResult[T] = {
    val op = implicitly[Numeric[T]]
    factorial(op.minus(x, op.one))
  }

  val SqrtPI = math.sqrt(math.Pi)

  /**
   * See https://en.wikipedia.org/wiki/Particular_values_of_the_gamma_function
   */
  def gammaOfHalfPlus[T: Integral](x: T): Option[Double] = {
    val op          = implicitly[Numeric[T]]
    val factorial2n = factorial(op.times(x, op.fromInt(2)))

    for {
      fac2n <- factorial2n
      facn  <- factorial(x)
    } yield {
      val numerator   = op.toDouble(fac2n)
      val n           = op.toDouble(x)
      val denominator = math.pow(4, n) * op.toDouble(facn)
      (numerator / denominator) * SqrtPI
    }
  }

}
