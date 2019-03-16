package uk.co.odinconsultants.smaths.stats

/**
  * Data from https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence
  */
trait KullbackLeiblerFixture {

  val ps          = Seq(0.36, 0.48, 0.16)
  val qs          = Seq(0.333, 0.333, 0.333)
  val KL_Q_P      = 0.09637
  val tolerance   = 0.0001
}
