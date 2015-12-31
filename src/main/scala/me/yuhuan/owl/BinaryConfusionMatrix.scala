package me.yuhuan.owl

/**
  *                 ┌─────────────────┬─────────────────┐
  *                 │  Auto says NO   │  Auto says YES  │
  * ┌───────────────┼─────────────────┼─────────────────┤
  * │ Gold says No  │ True Negatives  │ False Positives │
  * ├───────────────┼─────────────────┼─────────────────┤
  * │ Gold says Yes │ False Negatives │ True Positives  │
  * └───────────────┴─────────────────┴─────────────────┘
  *
  * @see http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
  *
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 12/29/15.
  */
class BinaryConfusionMatrix(val tp: Double, val tn: Double, val fp: Double, val fn: Double) {
  def accuracy = if (tp + tn == 0.0) 0.0 else (tp + tn) / (tp + tn + fp + fn)
  def precision = if (tp == 0.0) 0.0 else tp / (fp + tp)
  def recall = if (tp == 0.0) 0.0 else tp / (fn + tp)
  def f1 = if (tp == 0.0) 0.0 else 2 * tp / (2 * tp + fp + fn)
  def sensitivity = recall
  def specificity = if (tn == 0.0) 0.0 else tn / (tn + fp)
  def prevalence = (fn + tp) / (tp + tn + fp + fn)
}
