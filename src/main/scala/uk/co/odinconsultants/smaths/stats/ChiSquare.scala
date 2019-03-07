package uk.co.odinconsultants.smaths.stats

import scala.reflect.ClassTag

object ChiSquare {

  import uk.co.odinconsultants.smaths.linalg.Tensors._

  def columnMeans[T : Numeric : ClassTag](m: Matrix[T]): Vec[Double] = {
    val nCols = m(0).length
    val op    = implicitly[Numeric[T]]
    val added = m.foldLeft(Array.fill(nCols)(op.zero)) { case (agg, xs) => add(agg, xs) }
    val n     = m.length
    added.map(op.toDouble(_) / n)
  }

  def chiSquare[T](xs: Matrix[T]): Double = {
    ???
  }

}
