package uk.co.odinconsultants.smaths.linalg

import scala.reflect.ClassTag

object Tensors {

  type Vec[T] = Array[T]

  type Matrix[T] = Array[Array[T]]

  def add[T : Numeric : ClassTag](xs: Vec[T], ys: Vec[T]): Vec[T] = {
    val op = implicitly[Numeric[T]]
    xs.zip(ys).map { case (x, y) => op.plus(x, y) }
  }

}
