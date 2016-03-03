package qa.qtype

import java.io.File

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.learning.{LogisticRegressionClassifier, RVFDatum, RVFDataset}
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.Counter
import qa.input.{Question, InputReader}
import qa.voting.Voter._

/**
  * Created by bsharp on 3/3/16.
  */


object BottomUpClassify extends App {

  lazy val processor = new FastNLPProcessor()

  // Opens a file, reads the questions and divides them into train, dev, and test partitions
  def generateFoldsFromFile(fn:String):(Seq[Question], Seq[Question], Seq[Question]) = {
    val questions = new InputReader(new File(fn)).qTypeQuestions
    val numQuestions = questions.length
    // Grab the first 60% questions for train
    val (train, other) = questions.partition(questions.indexOf(_) <= (0.6*numQuestions))
    val numOther = other.length
    // Divide the rest into dev and test
    val (dev, test) = other.partition(other.indexOf(_) <= (0.5*numOther))

    println (s"After partitioning the $numQuestions questions...")
    println (s"Train Questions: ${train.length}")
    println (s"Dev Questions: ${dev.length}")
    println (s"Test Questions: ${test.length}")

    (train, dev, test)
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
    val qDoc = processor.annotate(q.question)
    val aDocs = q.choices.map(a => processor.annotate(a.text))
    val fc = new Counter[String]
    // todo Turn on/Off Features?
    // TODO Make Features!
    // Length Features:
    val numSentsinQuestion = qDoc.sentences.length
    fc.setCount("numSentsInQuestion", numSentsinQuestion)

    val wordsInAns = for {
      aDoc <- aDocs
      s <- aDoc.sentences
    } yield s.words.length
    val avgWordsInAns:Double = wordsInAns.sum.toDouble / aDocs.size.toDouble
    fc.setCount("avgWordsInEachAnswer", avgWordsInAns)

    // TODO Unigram/Bigram features?

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
  val (trainQuestions, devQuestions, testQuestions) = generateFoldsFromFile(config.getString("buc.questions"))
  val trainDataset = mkRVFDataset(trainQuestions)

  // 3. Train
  val classifier = new LogisticRegressionClassifier[Int, String]()
  classifier.train(trainDataset)

  // 4. Predict
  val useDev = config.getBoolean("buc.useDev")
  if (useDev) println ("Testing on DEV") else ("Testing on **TEST**")
  val testDatums = if (useDev) devQuestions.map(q => mkDatumFromQuestion(q)) else testQuestions.map(q => mkDatumFromQuestion(q))
  val predictedLabels = testDatums.map(td => classifier.classOf(td))

  // 5. Evaluate
  val precisionAt1 = evaluate[Int,String](testDatums, predictedLabels)
  println ("Final Precision: " + precisionAt1)


}
