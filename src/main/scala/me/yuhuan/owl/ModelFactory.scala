package me.yuhuan.owl

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait ModelFactory[-X, -Y, +M] {
  def newBuilder: ModelBuilder[X, Y, M]

  def fitOn(xs: Iterable[X], ys: Iterable[Y]): M = {
    val b = newBuilder
    b.add(xs, ys)
    b.result
  }

  def fitOn(xys: Iterable[(X, Y)]): M = {
    val (xs, ys) = xys.unzip
    fitOn(xs, ys)
  }
}
