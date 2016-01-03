package me.yuhuan.owl
import scala.collection._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class ConfusionMatrix[V](val counts: Map[(V, V), Int], val labels: Set[V]) { self =>
  def apply(goldAuto: (V, V)): Int = if (counts.contains(goldAuto)) counts(goldAuto) else 0
  def apply(gold: V, auto: V): Int = if (counts.contains(gold -> auto)) counts(gold -> auto) else 0

  /**
    * Combines two confusion matrices by adding corresponding results.
    */
  def +(that: ConfusionMatrix[V]): ConfusionMatrix[V] = {
    require(
      self.labels == that.labels,
      "The two confusion matrices must be using the same set of labels."
    )

    val newCount = mutable.HashMap[(V, V), Int]()
    for (v1 <- labels; v2 <- labels) {
      val k = v1 -> v2
      newCount(k) = self(k) + that(k)
    }


    // TODO: this lazy version is slow

//    val newCount = new DefaultMap[(V, V), Int] {
//      def get(k: (V, V)): Option[Int] = {
//        if (self.labels.contains(k._1) && self.labels.contains(k._2)) {
//          Some(self(k) + that(k))
//        }
//        else None
//      }
//      def iterator: scala.Iterator[((V, V), Int)] = new scala.Iterator[((V, V), Int)] {
//        val ls = self.labels.toIndexedSeq
//        var i = 0
//        var j = 0
//        def inc() = {
//          j += 1
//          if (j == ls.length) {
//            j = 0
//            i += 1
//          }
//        }
//        def hasNext: Boolean = i < ls.length && j < ls.length
//        def next(): ((V, V), Int) = {
//          val k = ls(i) -> ls(j)
//          println(i + "," + j)
//          inc()
//          (k, get(k).get)
//        }
//      }
//    }
    new ConfusionMatrix[V](newCount, labels)
  }

  /**
    * Suppose T1 is chosen as the value of interest (i.e., the value that will be considered TRUE)
    *                     ┌────┐
    *               Auto  │Auto│  Auto   Auto
    *               = T0  │= T1│  = T2   = T3
    *                     └────┘
    *             ┌──────┬──────┬──────┬──────┐
    *  Gold = T0  │  TN  │  FP  │  TN  │  TN  │
    * ┌─────────┐ ├──────┼──────┼──────┼──────┤
    * │Gold = T1│ │  FN  │  TP  │  FN  │  FN  │
    * └─────────┘ ├──────┼──────┼──────┼──────┤
    *  Gold = T2  │  TN  │  FP  │  TN  │  TN  │
    *             ├──────┼──────┼──────┼──────┤
    *  Gold = T3  │  TN  │  FP  │  TN  │  TN  │
    *             └──────┴──────┴──────┴──────┘
    *
    */
  def binarized(valueOfInterest: V): BinaryConfusionMatrix = {
    val valuesDespised = self.labels.filter(_ != valueOfInterest).toSeq
    val tp = self(valueOfInterest -> valueOfInterest)
    val tn = (for (v1 <- valuesDespised; v2 <- valuesDespised) yield v1 -> v2).map(self.apply).sum
    val fp = valuesDespised.map(v => self(v -> valueOfInterest)).sum
    val fn = valuesDespised.map(v => self(valueOfInterest -> v)).sum
    new BinaryConfusionMatrix(tp, tn, fp, fn)
  }
}

object ConfusionMatrix {
  def of[K, V](gold: Map[K, V], auto: Map[K, V], labels: Set[V]): ConfusionMatrix[V] = {
    require(
      (gold.values ++ auto.values).forall(labels.contains),
      s"The gold and auto labels must use the labels from the provided label set {${labels.mkString(",")}}!"
    )

    require( // TODO: Change to automatically counting them as FNs or FPs
      gold.keys.size == auto.keys.size,
      "The number of auto predictions should match that of the gold! "
    )

    val commonKeys = gold.keySet intersect auto.keySet
    val counts = mutable.HashMap[(V, V), Int]()
    commonKeys.foreach { k =>
      val v1 = gold(k)
      val v2 = auto(k)
      if (counts.contains(v1 -> v2)) counts(v1 -> v2) += 1
      else counts(v1 -> v2) = 1
    }
    new ConfusionMatrix[V](counts.toMap, labels)
  }
}

object ConfusionMatrixTest extends App {

  val cm1 = ConfusionMatrix.of(
    auto = Map("k1" -> "+", "k2" -> "–", "k3" -> "+", "k4" -> "0"),
    gold = Map("k1" -> "+", "k2" -> "+", "k3" -> "–", "k4" -> "+"),
    labels = Set("+", "–", "0")
  )

  val cm2 = ConfusionMatrix.of(
    auto = Map("k1" -> "+", "k2" -> "–", "k3" -> "+", "k4" -> "0"),
    gold = Map("k1" -> "–", "k2" -> "–", "k3" -> "0", "k4" -> "+"),
    labels = Set("+", "–", "0")
  )

  val cms = Array.fill[ConfusionMatrix[String]](100)(cm1)
  val sum = cms.reduce(_ + _)

  val bp = 0
}