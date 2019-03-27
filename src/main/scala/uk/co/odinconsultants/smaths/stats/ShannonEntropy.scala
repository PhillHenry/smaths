package uk.co.odinconsultants.smaths.stats

import uk.co.odinconsultants.smaths.arithmetic.Logs.logBase2

object ShannonEntropy {

  def entropyOf(p: Double): Double = - p * logBase2(p)

}
