package qa.voting

import qa.input._
import scala.collection.mutable.ArrayBuffer

/**
  * Created by bsharp on 2/12/16.
  */
class EnsembleUtils {

}

object EnsembleUtils {


  def rankPrecisions(questions: Seq[Question], scores: Seq[Seq[Double]]): Seq[Double] = {
    val numQuestions = questions.length
    val numRanks = scores.head.length
    val out = new Array[Double](numRanks)

    // Iterate through scores and find the correctness of each score as rank
    // Array[(rank:Int, correct:Double)] -- four for each questions
    val allScoresAsRanks = new ArrayBuffer[(Int, Double)]
    for (i <- 0 until numQuestions) {
      val goldAnswer = questions(i).rightChoice.getOrElse(0)
      val currScores = scores(i)
      val sorted = currScores.zipWithIndex.sortBy(- _._1)
      var rank:Int = 0
      val sortedWithRanksAfterTieHandling = new ArrayBuffer[((Double, Int), Int)]
      // Add top-ranked
      sortedWithRanksAfterTieHandling.append((sorted.head, rank))
      // Add the others, handling ties
      for (i <- 1 until sorted.length) {
        val (currScore, index) = sorted(i)
        val prevScore = sortedWithRanksAfterTieHandling(i - 1)._1._1
        if (currScore <= prevScore) {
          // Not a tie, increment rank
          rank += 1
        }
        sortedWithRanksAfterTieHandling.append(((currScore, index), rank))
      }
      assert (sortedWithRanksAfterTieHandling.length == sorted.length)

      for {
        ((s, index), rank) <- sortedWithRanksAfterTieHandling
        correct = if (index == goldAnswer) 1.0 else 0.0
      } allScoresAsRanks.append((rank, correct))
    }

    // Group the ranks to check one at a time
    val ranksGrouped = allScoresAsRanks.groupBy(_._1)

    for (rank <- 0 until ranksGrouped.keySet.size) {
      val rankItems = ranksGrouped(rank)
      val numInRank = rankItems.length.toDouble
      val numCorrect = rankItems.unzip._2.sum
      val rankPrecision = (numCorrect / numInRank) * 100
      out(rank) = rankPrecision
    }

    // Display the results
    println ("Precision at rank: ")
    for (rank <- 0 until numRanks) {
      println ("\tRank " + rank + ": " + out(rank))
    }

    out.toSeq
  }



}
