package qa

import java.io.PrintWriter
import java.util.Properties

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.Lexicon
import edu.arizona.sista.utils.StringUtils

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by bsharp on 2/3/16.
  */
class Voter {}

object Voter {
  val processor = new FastNLPProcessor(withDiscourse = false)
  val questionFilter = new QuestionFilter

  def castVotes(questions: Array[KaggleQuestion], rankers:Array[Ranker], method:String): Array[(Int, Int)] = {

    val numQuestions = questions.length
    val selections = new Array[(Int, Int)](numQuestions)
    var precisionAt1:Double = 0.0

    println (s"Casting votes for $numQuestions questions from ${rankers.length} rankers...")

    // For each question
    for (i <- questions.indices) {
      val qid = questions(i).id
      val correct = questions(i).correct
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

  def getVotes(scoresIn:Array[Double], votingMethod:String, voteScale:Array[Double]):Array[Double] = {
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
  def getSelectedAnswers(in:Array[Double]):Array[Int] = {
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

  def findTies(scores:Array[Double]):(Array[(Array[Int], Int)], Array[(Double, Int)]) = {
    val sorted = scores.zipWithIndex.sortBy(- _._1)
    val ties = findTies(sorted)
    (ties, sorted)
  }


  def findTies(sortedScores:Array[(Double, Int)]):Array[(Array[Int], Int)] = {
    val out = new ArrayBuffer[(Array[Int], Int)]

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

    out.toArray
  }



  /*
   * Loading/Saving methods
   */

  def saveSubmissionCSV (selections: Array[(Int, Int)], filename:String): Unit = {
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

  def parseTSV (tsv:String, lexicon: Lexicon[Int]): Array[Array[Double]] = {
    println ("Loading scores from " + tsv)
    val answerChoices = Array("A", "B", "C", "D")
    val out = new ArrayBuffer[Array[Double]]

    val lines = scala.io.Source.fromFile(tsv, "UTF-8").getLines().toList

    var questionCounter:Int = 0

    for (line <- lines) {
      //println (line)
      val fields = line.split("\t")
      assert (fields.length == 3)
      var qid = fields(0).toInt
      // If the tsv is in the Sia format - replace the qIndex with the qID
      if (qid > 9000) qid = lexicon.add(qid)

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

    out.toArray
  }


  def loadQuestions (filename:String): Array[KaggleQuestion] = {

    println ("Loading questions from " + filename + "...")
    val out = new ArrayBuffer[KaggleQuestion]

    val answerLabels = Array("A", "B", "C", "D")

    val lines = scala.io.Source.fromFile(filename, "UTF-8").getLines().toList
    for (line <- lines.slice(1, lines.length)) {
      //println (line)
      val fields = line.split("\t")
      val qid = fields(0).toInt
      val qText = fields(1)
      val correct = if (fields.length == 7) answerLabels.indexOf(fields(2)) else -1
      val answerTexts = if (fields.length == 7) fields.slice(3, 7) else fields.slice(2, 6)
      val answers = answerTexts.map(s => new Answer(s))
      out.append(new KaggleQuestion(qid, qText, correct, answers))
    }

    println ("Finished loading " + out.length + " questions.")
    out.toArray
  }

  def loadRankers (props:Properties, lexicon: Lexicon[Int], questions: Array[KaggleQuestion]): Array[Ranker] = {
    val rankers = new ArrayBuffer[Ranker]

    val nRankers = StringUtils.getInt(props, "maxrankers", 10)
    for (i <- 1 to nRankers) {
      val enabled = StringUtils.getBool(props, s"ranker.$i.enabled", false)
      val rankerPrefix = props.getProperty(s"ranker.$i.prefix", "NULL_PREFIX")
      if (enabled) {
        val rankerTSVFile = props.getProperty(s"ranker.$i.tsv")
        val rankerScores = parseTSV(rankerTSVFile, lexicon)
        val currRanker = new Ranker(rankerScores, rankerPrefix)

        // Find the voting scale for the ranker
        val rankerDevTSVFile = props.getProperty(s"ranker.$i.tsv.dev")
        val rankerDevScores = parseTSV(rankerDevTSVFile, lexicon)
        currRanker.votingScale = EnsembleUtils.rankPrecisions(questions, rankerDevScores)

        rankers.append(currRanker)
        println (s"Appended Ranker $i: $rankerPrefix")
      }
    }

    rankers.toArray
  }

  def buildQIDLexicon (questions: Array[KaggleQuestion]): Lexicon[Int] = {
    val lexicon = new Lexicon[Int]
    for (q <- questions) {
      lexicon.add(q.id)
      //println ("Added " + q.id + " to lexicon.")
    }

    lexicon
  }



  def main(args:Array[String]): Unit = {
    val props = StringUtils.argsToProperties(args)

    val questions = loadQuestions(props.getProperty("questions"))
    // The dev questions should be the 506, and should align with the ranker.i.tsv.dev ranking file for determining the
    // voting scale
    val devQFilename = props.getProperty("questions.dev", "NONE")
    // If you are running only over the 506, then these are just the regular questions
    val devQuestions = if (devQFilename == "NONE") questions else loadQuestions(devQFilename)
    val qIDLexicon = buildQIDLexicon(questions)

    // Loads up the rankers and also calculates the voting scale based on the 506 dev questions
    val rankers = loadRankers(props, qIDLexicon, devQuestions)

    // Display the rank precision for each ranker:
    for (r <- rankers) {
      println ("Rank precision for " + r.name + "... [" + r.votingScale.mkString(", ") + "]")
    }

    val method = props.getProperty("method")
    val selections = castVotes(questions, rankers, method)

    // Save the selections in the correct format
    val submissionCSVFilename = props.getProperty("submission_filename")
    saveSubmissionCSV(selections, submissionCSVFilename)



  }
}

class Ranker (val scores: Array[Array[Double]], val name: String = "noname") {
  var votingScale = Array.fill[Double](4)(0.0)
}

class KaggleQuestion(val id:Int, val text:String, val correct:Int, val choices:Array[Answer])

class Answer (val text:String)