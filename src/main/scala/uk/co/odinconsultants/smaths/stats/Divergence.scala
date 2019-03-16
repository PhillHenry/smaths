package uk.co.odinconsultants.smaths.stats

object Divergence {

  def kl(p: Double, q: Double): Double = p * math.log(p / q)

}
