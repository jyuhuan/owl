package me.yuhuan.owl
import scala.collection.mutable

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class ConfusionMatrix[V](counts: Map[(V, V), Int], val labels: Set[V]) { outer =>
  def apply(goldAuto: (V, V)): Int = if (counts.contains(goldAuto)) counts(goldAuto) else 0
  def apply(gold: V, auto: V): Int = if (counts.contains(gold -> auto)) counts(gold -> auto) else 0


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
    val valuesDespised = outer.labels.filter(_ != valueOfInterest).toSeq
    val tp = outer(valueOfInterest -> valueOfInterest)
    val tn = (for (v1 <- valuesDespised; v2 <- valuesDespised) yield v1 -> v2).map(outer.apply).sum
    val fp = valuesDespised.map(v => outer(v -> valueOfInterest)).sum
    val fn = valuesDespised.map(v => outer(valueOfInterest -> v)).sum
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

  val cm = ConfusionMatrix.of(
    //                 TP           FN           FP           FN
    auto = Map("k1" -> "+", "k2" -> "–", "k3" -> "+", "k4" -> "0"),
    gold = Map("k1" -> "+", "k2" -> "+", "k3" -> "–", "k4" -> "+"),
    labels = Set("+", "–", "0")
  )

  val bcm = cm.binarized("+")

  val bp = 0
}