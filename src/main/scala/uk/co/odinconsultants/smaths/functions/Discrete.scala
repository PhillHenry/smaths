package uk.co.odinconsultants.smaths.functions

import uk.co.odinconsultants.smaths.Result.IntegralResult
import uk.co.odinconsultants.smaths.arithmetic.Factorial.factorial

object Discrete {

  def gamma[T: Integral](x: T): IntegralResult[T] = {
    val op = implicitly[Numeric[T]]
    factorial(op.minus(x, op.one))
  }

}
