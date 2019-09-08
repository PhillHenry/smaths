package uk.co.odinconsultants.smaths

object Result {

  type IntegralResult[T] = Option[T]

  def errorIn[T: Numeric](x: IntegralResult[T]): Boolean = x match {
    case None => true
    case _ => false
  }

}
