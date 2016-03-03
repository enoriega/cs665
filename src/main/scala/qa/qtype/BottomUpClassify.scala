package qa.qtype

import java.io.File

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.learning.{LogisticRegressionClassifier, RVFDatum, RVFDataset}
import edu.arizona.sista.struct.Counter
import qa.input.{Question, InputReader}
import qa.voting.Voter._

/**
  * Created by bsharp on 3/3/16.
  */


object BottomUpClassify extends App {

  // Opens a file, reads the questions and divides them into train, dev, and test partitions
  def generateFoldsFromFile(fn:String):(Seq[Question], Seq[Question]) = {
    val questions = new InputReader(new File(fn)).qTypeQuestions
    ???
  }

  // Make a Dataset for training out of the training questions
  def mkRVFDataset(questions:Seq[Question]):RVFDataset[Int, String] = {
    val dataset = new RVFDataset[Int, String]()

    // For each question, make a datum
    for (q <- questions) dataset += mkDatumFromQuestion(q)

    dataset
  }

  // Given a Question, extract the features and return a Datum
  def mkDatumFromQuestion(q:Question):RVFDatum[Int, String] = {
    // Get label
    val label = q.rightChoice.getOrElse(-1)
    assert (label != -1)
    // Extract Features
    val questionFeatures = mkFeatures(q)

    new RVFDatum[Int, String](label, questionFeatures)
  }

  // Extract a set of features from the question
  def mkFeatures(q:Question):Counter[String] = {
    val fc = new Counter[String]
    // todo Turn on/Off Features?
    // TODO Make Features!

    fc
  }

  // Determine how many of the predictions were correct
  def evaluate[L,F](datums:Seq[RVFDatum[L,F]], predictedLabels:Seq[L]): Double = {
    var precisionAt1:Double = 0.0
    val numDatums:Double = datums.length.toDouble

    // Increment the P@1 if we get the label correct
    // TODO: not handling ties, but currently not an issue (only two labels)
    for {
      i <- datums.indices
      gold = datums(i).label
      if predictedLabels(i) == gold
    } precisionAt1 += (1.0 / numDatums)

    precisionAt1
  }

  // Main method
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  // 1. Load the training dataset
  val (trainQuestions, testQuestions) = generateFoldsFromFile(config.getString("buc.questions"))
  val trainDataset = mkRVFDataset(trainQuestions)

  // 3. Train
  val classifier = new LogisticRegressionClassifier[Int, String]()
  classifier.train(trainDataset)

  // 4. Predict
  val testDatums = testQuestions.map(q => mkDatumFromQuestion(q))
  val predictedLabels = testDatums.map(td => classifier.classOf(td))

  // 5. Evaluate
  val precisionAt1 = evaluate[Int,String](testDatums, predictedLabels)
  println ("Final Precision: " + precisionAt1)


}
