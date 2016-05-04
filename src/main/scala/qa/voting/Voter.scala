package qa.voting

import java.io.PrintWriter
import java.util.Properties

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.Lexicon
import edu.arizona.sista.utils.StringUtils

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.io.File
import com.typesafe.config.{ConfigFactory, Config}
import qa.input._


/**
  * Created by bsharp on 2/3/16.
  */


object Voter extends App {
  //val processor = new FastNLPProcessor(withDiscourse = false)
  val questionFilter = new QuestionFilter

  def castVotes(questions: Seq[Question], rankers:Seq[Ranker], method:String): Seq[(Int, Int)] = {

    val numQuestions = questions.length
    val selections = new Array[(Int, Int)](numQuestions)
    var precisionAt1:Double = 0.0

    println (s"Casting votes for $numQuestions questions from ${rankers.length} rankers...")

    // For each question
    for (i <- questions.indices) {
      val qid = questions(i).id
      val correct = questions(i).rightChoice.getOrElse(0)
      val numAns = questions(i).choices.length

      val filterOut = questionFilter.mainFilter(questions(i))

      if (filterOut) {
        println (s"filtering out question $i")
        selections(i) = (qid, 3)

        if (correct == 3) {
          precisionAt1 += 1.0 / numQuestions.toDouble
          println ("Filtered question was answered CORRECTLY")
        } else {
          println ("Filtered question was answered INCORRECTLY")
        }

      } else {
        val voteTally = Array.fill[Double](numAns)(0.0)

        // Have rankers cast votes
        for (rID <- rankers.indices) {
          val ranker = rankers(rID)
          val scores = ranker.scores(i)
          val votes = getVotes(scores, method, ranker.votingScale)
//          val votes = getVotes(scores, method, Array(4.0, 3.0, 2.0, 1.0))
          // Add the vote(s) for the ranker to the overall tally for the question
          for (j <- votes.indices) voteTally(j) += votes(j)
        }

        // Tally votes, look for ties, choose winner
        val chosen = new ArrayBuffer[Int]
        val sortedVotes = voteTally.zipWithIndex.sortBy(- _._1)
        val maxVote = sortedVotes.head._1
        for (sv <- sortedVotes) {
          if (sv._1 == maxVote) chosen.append(sv._2)
        }

        // Select and Store answer -- if tie - select at random?
        // Case 1: No tie --
        if (chosen.length == 1) {
          selections(i) = (qid, chosen.head)
        }
        // When there's a tie -- choose randomly (can change)
        else if (chosen.length > 1) {
          val randomInt = Random.nextInt(chosen.length)
          selections(i) = (qid, chosen(randomInt))
        }
        // Catch error
        else {
          throw new RuntimeException ("ERROR: no answer is selected for question " + i)
        }

        // Calculate the P@1 for question:
        if (chosen.contains(correct)) precisionAt1 += (1.0 / chosen.length.toDouble) / numQuestions.toDouble
        //else println (s"Failed to answer question $i correctly.  (Chosen = ${chosen.mkString(",")}, correct = ${correct})")

      }

    }

    println ("Finished casting votes...")
    println ("P@1 = " + precisionAt1.formatted("%3.3f"))

    selections
  }

  def getVotes(scoresIn:Seq[Double], votingMethod:String, voteScale:Seq[Double]):Seq[Double] = {
    val votes = new Array[Double](scoresIn.size)
    //val voteScale = Array[Double](4.0, 3.0, 2.0, 1.0)

    val (ties, sorted) = findTies(scoresIn)
    val topScoreIndices = getSelectedAnswers(scoresIn)

    // Vote
    if (votingMethod == "single") {
      for (index <- topScoreIndices) {
        votes(index) = 1.0 / topScoreIndices.length.toDouble
      }
    }

    else if (votingMethod == "multiple") {

      for (rank <- sorted.indices) {
        // Base case: no ties
        var votesCast: Double = voteScale(rank)

        val currScoreIndex = sorted(rank)._2
        // Check to see if that rank is in a tie
        for (tieIndex <- ties.indices) {
          val tiesInGroup = ties(tieIndex)._1
          if (tiesInGroup.contains(currScoreIndex)) {
            // If the rank is involved in a tie
            val tieSize = tiesInGroup.size
            val tieStartOffset = ties(tieIndex)._2
            val votesToSplit = voteScale.slice(tieStartOffset, tieStartOffset + tieSize).sum
            votesCast = votesToSplit / tieSize.toDouble
          }
        }
        votes(currScoreIndex) = votesCast
      }
    }

    votes
  }

  // Returns all of the indices that are tied for the top answer score
  def getSelectedAnswers(in:Seq[Double]):Seq[Int] = {
    val sorted = in.zipWithIndex.sortBy(- _._1)
    // Check for ties
    val topScore = sorted(0)._1
    val topScoreIndices = new ArrayBuffer[Int]
    for (i <- 0 until sorted.size) {
      if (sorted(i)._1 == topScore) {
        topScoreIndices.append(sorted(i)._2)
      }
    }
    topScoreIndices.toArray
  }

  def findTies(scores:Seq[Double]):(Seq[(Seq[Int], Int)], Seq[(Double, Int)]) = {
    val sorted = scores.zipWithIndex.sortBy(- _._1)
    val ties = findTies(sorted)
    (ties, sorted)
  }


  def findTies(sortedScores:Seq[(Double, Int)]):Seq[(Seq[Int], Int)] = {
    val out = new ArrayBuffer[(Seq[Int], Int)]

    var i:Int = 0
    var tieGroupOffset:Int = 0
    //for (i <- 0 until sorted.size) {
    while (i < sortedScores.size) {
      val currScore = sortedScores(i)._1
      val currIndex = sortedScores(i)._2
      val runningTies = new ArrayBuffer[Int]
      runningTies.append(currIndex)
      for (j <- (i + 1) until sortedScores.size) {
        val nextScore = sortedScores(j)._1
        val nextIndex = sortedScores(j)._2
        if (nextScore == currScore) {
          if (runningTies.size == 1) {
            tieGroupOffset = i
          }
          runningTies.append(nextIndex)
          i = j
        }
      }

      // If I found any ties, save them
      if (runningTies.size > 1) {

        out.append((runningTies.toArray, tieGroupOffset))
      }

      // Increment i
      i += 1
    }

    out.toSeq
  }



  /*
   * Loading/Saving methods
   */

  def saveSubmissionCSV (selections: Seq[(Int, Int)], filename:String): Unit = {
    val answerLabels = Array("A", "B", "C", "D")

    val pw = new PrintWriter(filename)
    for (s <- selections) {
      val qid = s._1
      val ansIndex = s._2
      pw.println (s"$qid,${answerLabels(ansIndex)}")
    }
    pw.flush()
    pw.close()
  }

  def parseTSV (tsv:String, lexicon: Lexicon[Int]): Seq[Seq[Double]] = {
    println ("Loading scores from " + tsv)
    val answerChoices = Array("A", "B", "C", "D")
    val out = new ArrayBuffer[Array[Double]]

    val lines = scala.io.Source.fromFile(tsv, "UTF-8").getLines().toList

    var questionCounter:Int = 0

    var qidIsKaggle: Boolean = true

    for (line <- lines) {
      val fields = line.split("\t")
      if (fields.length != 3) println(line)
      assert (fields.length == 3)

      var qid = fields(0).toInt
      // If the tsv is in the Sia format - take note
      if (qid == 0) qidIsKaggle = false

      if (qidIsKaggle) qid = lexicon.add(qid)

      val aidRaw = fields(1)
      val aid = if (answerChoices.contains(aidRaw)) answerChoices.indexOf(aidRaw) else aidRaw.toInt
      val score = fields(2).toDouble

      // Add a new "row" for next question
      if (qid >= out.length) {
        out.append(new Array[Double](4))
        questionCounter += 1
      }

      if (aid < 4) out(qid)(aid) = score

    }

    out.toSeq map (_.toSeq)
  }

  def loadRankers (conf:Config, lexicon: Lexicon[Int], questions: Seq[Question]): Seq[Ranker] = {
    val rankers = new ArrayBuffer[Ranker]

    val nRankers = config.getInt("voter.maxRankers")
    for (i <- 1 to nRankers) {
      val enabled = config.getBoolean(s"voter.rankers.$i.enabled")
      val rankerPrefix = config.getString(s"voter.rankers.$i.prefix")
      if (enabled) {
        val rankerTSVFile = config.getString(s"voter.rankers.$i.tsv")
        val rankerScores = parseTSV(rankerTSVFile, lexicon)
        val currRanker = new Ranker(rankerScores, rankerPrefix)

        // Find the voting scale for the ranker
        val rankerDevTSVFile = config.getString(s"voter.rankers.$i.tsv_dev")
        val rankerDevScores = parseTSV(rankerDevTSVFile, lexicon)
        currRanker.votingScale = EnsembleUtils.rankPrecisions(questions, rankerDevScores)

        rankers.append(currRanker)
        println (s"Appended Ranker $i: $rankerPrefix")
      }
    }
    rankers
  }

  def buildQIDLexicon (questions: Seq[Question]): Lexicon[Int] = {
    val lexicon = new Lexicon[Int]
    for (q <- questions) {
      lexicon.add(q.id)
      //println ("Added " + q.id + " to lexicon.")
    }

    lexicon
  }



  // Main method
  val config =
      if (args.isEmpty) ConfigFactory.load()
      else ConfigFactory.parseFile(new File(args(0))).resolve()

  val questions = new InputReader(new File(config.getString("voter.questions"))).questions
  // The dev questions should be the 506, and should align with the ranker.i.tsv.dev ranking file for determining the
  // voting scale
  // If you are running only over the 506, then these are just the regular questions
  val devQuestions = new InputReader(new File(config.getString("voter.questions_dev"))).questions
  val qIDLexicon = buildQIDLexicon(questions)

  // Loads up the rankers and also calculates the voting scale based on the 506 dev questions
  val rankers = loadRankers(config, qIDLexicon, devQuestions)
  // Display the rank precision for each ranker:
  for (r <- rankers) {
    println ("Rank precision for " + r.name + "... [" + r.votingScale.mkString(", ") + "]")
  }

  // Vote
  val method = config.getString("voter.method")
  val selections = castVotes(questions, rankers, method)

  // Save the selections in the correct format
  val submissionCSVFilename = new File(config.getString("voter.submissionFilename"))
  saveSubmissionCSV(selections, submissionCSVFilename.getCanonicalPath)

}

class Ranker (val scores: Seq[Seq[Double]], val name: String = "noname") {
  var votingScale:Seq[Double] = Array.fill[Double](4)(0.0)
}
