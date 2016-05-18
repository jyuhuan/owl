package me.yuhuan.owl

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */

trait Evaluator[-Y, Metric] {
  def eval(auto: Iterable[Y], gold: Iterable[Y]): Metric

}
