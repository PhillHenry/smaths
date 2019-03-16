package uk.co.odinconsultants.smaths.linalg

import scala.reflect.ClassTag
import math._

object Tensors {

  type Vec[T] = Array[T]

  type Matrix[T] = Vec[Vec[T]]

  def toVec[T : Numeric : ClassTag](t: T*): Vec[T] = t.toArray

  def add[T : Numeric : ClassTag](xs: Vec[T], ys: Vec[T]): Vec[T] = {
    val op = implicitly[Numeric[T]]
    xs.zip(ys).map { case (x, y) => op.plus(x, y) }
  }

  def addRows[T : Numeric : ClassTag](xs: Matrix[T]): Vec[T] = {
    val op    = implicitly[Numeric[T]]
    val n     = xs.head.length
    val empty = Array.fill(n)(op.zero)
    xs.foldLeft(empty)(add)
  }

  def l1Normalize[T : Numeric : ClassTag](xs: Vec[T]): Vec[Double] = {
    val op    = implicitly[Numeric[T]]
    val total = op.toDouble(xs.sum)
    xs.map(op.toDouble(_) / total)
  }

  def l2Normalize[T : Numeric : ClassTag](xs: Vec[T]): Vec[Double] = {
    val op      = implicitly[Numeric[T]]
    val squared = xs.map(x => op.times(x, x))
    val total   = op.toDouble(squared.sum)
    xs.map(op.toDouble(_) / pow(total, 0.5))
  }

}
