package me.yuhuan.owl

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Model[-X, +Y] extends (X => Y) {
  def apply(xs: Iterable[X]): Iterable[Y] = xs.map(this)
}
