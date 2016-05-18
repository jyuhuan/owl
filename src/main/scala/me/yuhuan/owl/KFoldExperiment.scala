package me.yuhuan.owl

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class KFoldExperiment[X, Y, Mo <: Model[X, Y], Me](
                                                    k: Int,
                                                    mf: ModelFactory[X, Y, Mo],
                                                    xys: Seq[(X, Y)],
                                                    evaluator: Evaluator[Y, Me],
                                                    aggregate: (Me, Me) => Me
                                                  ) {

  case class Fold(trainingSamples: Seq[(X, Y)], testingSamples: Seq[(X, Y)])

  val (xs, gold) = xys.unzip

  val foldSize =  xs.length.toDouble / k

  val folds: Seq[Fold] = (0 to k).map { i =>
    val l = (i * foldSize).round.toInt
    val r = ((i + 1) * foldSize).round.toInt
    Fold(xys.slice(0, l) ++ xys.slice(r, xys.length), xys.slice(l, r))
  }

  val foldMes = folds.map { case Fold(trainingSamples, testingSamples) =>
      val m = mf.fitOn(trainingSamples)
      val auto = testingSamples.map { case (x, _) => m(x) }
      evaluator.eval(auto, gold)
  }

  val finalMe = foldMes.reduce(aggregate)

}
