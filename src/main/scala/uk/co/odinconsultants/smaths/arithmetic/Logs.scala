package uk.co.odinconsultants.smaths.arithmetic

object Logs {

  val logBase2: Double => Double = { x => log(2, x) }

  def log(base: Double, x: Double): Double =
    math.log(x) / math.log(base)

}
