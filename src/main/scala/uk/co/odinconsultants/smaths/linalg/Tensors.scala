package uk.co.odinconsultants.smaths.linalg

import scala.reflect.ClassTag
import math._

object Tensors {

  type Vec[T] = Array[T]

  type Matrix[T] = Vec[Vec[T]]

  def toVec[T : ClassTag](t: T*): Vec[T] = t.toArray

  def emptyVec[T : Numeric : ClassTag](n: Int): Vec[T] = {
    val op = implicitly[Numeric[T]]
    Array.fill(n)(op.zero)
  }

  def add[T : Numeric : ClassTag](xs: Vec[T], ys: Vec[T]): Vec[T] = {
    val op = implicitly[Numeric[T]]
    xs.zip(ys).map { case (x, y) => op.plus(x, y) }
  }

  def addRows[T : Numeric : ClassTag](xs: Matrix[T]): Vec[T] = {
    val op    = implicitly[Numeric[T]]
    val n     = xs.head.length
    xs.foldLeft(emptyVec[T](n))(add)
  }

  def l1Normalize[T : Numeric : ClassTag](xs: Vec[T]): Vec[Double] = {
    val op    = implicitly[Numeric[T]]
    val total = op.toDouble(xs.sum)
    safeNormalize(xs, total)
  }

  def l2Normalize[T : Numeric : ClassTag](xs: Vec[T]): Vec[Double] = {
    val op      = implicitly[Numeric[T]]
    val squared = xs.map(x => op.times(x, x))
    val total   = op.toDouble(squared.sum)
    val diviser = pow(total, 0.5)
    safeNormalize(xs, diviser)
  }

  private def safeNormalize[T: Numeric : ClassTag](xs: Vec[T], diviser: Double): Vec[Double]  = {
    val op      = implicitly[Numeric[T]]
    if (diviser == 0)
      xs.map(op.toDouble(_))
    else
      elementWiseDivide(xs, diviser)
  }

  def elementWiseDivide[T : Numeric : ClassTag](xs: Vec[T], d: Double): Vec[Double] = {
    val op      = implicitly[Numeric[T]]
    xs.map(op.toDouble(_) / d)
  }

}
