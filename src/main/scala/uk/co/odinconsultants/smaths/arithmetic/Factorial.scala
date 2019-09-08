package uk.co.odinconsultants.smaths.arithmetic

import uk.co.odinconsultants.smaths.Result.IntegralResult

import scala.annotation.tailrec

object Factorial {

  def factorial[T: Integral](x: T): IntegralResult[T] = {
    val op = implicitly[Numeric[T]]
    import op._

    @tailrec
    def next(x: T, acc: T): T = {
      if (lteq(x, fromInt(1))) acc else next(minus(x, one), times(x, acc))
    }

    if (lt(x, zero))
      None
    else
      Some(next(x, one))
  }

}
