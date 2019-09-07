package uk.co.odinconsultants.smaths.arithmetic

import scala.annotation.tailrec

object Factorial {

  def factorial[T: Integral](x: T): T = {
    val op    = implicitly[Numeric[T]]
    @tailrec
    def next(x: T, acc: T): T = {
      if (op.fromInt(1) == x) acc else next(op.minus(x, op.one), op.times(x, acc))
    }
    next(x, op.one)
  }

}
