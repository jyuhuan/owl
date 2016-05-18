package me.yuhuan.owl

import scala.collection.mutable

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait ModelBuilder[-X, -Y, +M] {
  def add(x: X, y: Y): Unit
  def add(xs: Iterable[X], ys: Iterable[Y]): Unit = for ((x, y) <- xs.view zip ys.view) add(x, y)
  def result: M
}
