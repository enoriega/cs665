package qa.voting

import scala.collection.mutable.ArrayBuffer

/**
  * Created by bsharp on 2/12/16.
  */
class EnsembleUtils {

}

object EnsembleUtils {


  def rankPrecisions(questions: Array[KaggleQuestion], scores: Array[Array[Double]]): Array[Double] = {
    val numQuestions = questions.length
    val numRanks = scores.head.length
    val out = new Array[Double](numRanks)

    // Iterate through scores and find the correctness of each score as rank
    // Array[(rank, correct)] -- four for each questions
    val allScoresAsRanks = new ArrayBuffer[(Int, Double)]
    for (i <- 0 until numQuestions) {
      val goldAnswer = questions(i).correct
      val currScores = scores(i)
      val sorted = currScores.zipWithIndex.sortBy(- _._1)
      for {
        ((s, index), rank) <- sorted.zipWithIndex
        correct = if (index == goldAnswer) 1.0 else 0.0
      } allScoresAsRanks.append((rank, correct))
    }

    // Group the ranks to check one at a time
    val ranksGrouped = allScoresAsRanks.groupBy(_._1)

    // For each rank, check to see how many were ranked correctly
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

    out
  }



}
