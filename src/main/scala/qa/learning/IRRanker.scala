package qa.learning

import scala.io.Source
import sys.process._
import java.io.File
import java.util.UUID
import qa.input._
import qa.ir._
import scala.collection.JavaConverters._
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import com.typesafe.config._

// IR model for reranking
// Assumes svm_rank is in the path
class IRRanker(config:Config) extends Ranker{

  // File with the weights of svm_rank
  var modelFile:File = null
  var normalizers:Option[Seq[Double]] = None

  // Runs svm_classify and resorts the list with the new order
  def rerank(questions:Seq[Question], index:IRIndex):Seq[Question] = {
    // Load the corresponding model depending on the index's name
    val modelFile = new File(config.getString(s"indexes.${index.name}.irModelFile"))
    this.load(modelFile)

    val (lines:Seq[String], x:Seq[Double]) = questions2svmRankLines(questions, index, this.normalizers)

    // Write them to a training file
    val classificationFile = File.createTempFile(UUID.randomUUID.toString, "rank")
    FileUtils.writeLines(classificationFile, lines.asJavaCollection)

    val outputFile = File.createTempFile(UUID.randomUUID.toString, "txt")

    // Call svm_rank_train
    val exitCode = s"svm_rank_classify ${classificationFile.getCanonicalPath} ${this.modelFile.getCanonicalPath} ${outputFile.getCanonicalPath}".!

    if(exitCode != 0){
      throw new RuntimeException("Error running svm_rank_classify!!")
    }

    // Fetch the output file and select a choice per question
    val results = io.Source.fromFile(outputFile).getLines.map(_.toDouble).grouped(4).toList

    for((q, r) <- questions zip results) yield {
      val prediction = r.indexOf(r.max)
      // Plug svm_rank numerical score into the choices of the question object
      val newChoices = q.choices.zip(r).map{case (choice, score) => Answer(choice.text, score)}
      Question(q.id, q.question, newChoices, Some(prediction))
    }
  }

  // Generates features out of the IR query
  def createFeatureVector(question:String,
       choice:String, queryRes:QueryResult):Seq[Double] = {
         val avg = queryRes.topDocs.map(_.score).sum / queryRes.docsInIndex

         // Bin the number of scores in quartiles
         val scores = queryRes.topDocs.map(_.score/queryRes.maxScore)
         val bins:Map[Int, Double] = scores.groupBy{s =>
            if(s <= .25)
                1
            else if(s <= .5)
                2
            else if(s <= .75)
                3
            else
                4
         }.mapValues(_.size.toDouble)

         val first:Double = if(bins.contains(1)) bins(1) else 0.0
         val second:Double = if(bins.contains(2)) bins(2) else 0.0
         val third:Double = if(bins.contains(3)) bins(3) else 0.0
         val fourth:Double = if(bins.contains(4)) bins(4) else 0.0

        Seq(queryRes.numResults, queryRes.maxScore, avg, first, second, third, fourth)
  }

  // Trains svm_rank with this questions given this index
  def train(questions:Seq[Question], index:IRIndex, outputFile:Option[File], normalizersFile:Option[File]){
    // Writes an svm_rank training file
    modelFile = outputFile match {
      case Some(f) => f
      case None => File.createTempFile(UUID.randomUUID.toString, "model")
    }

    // Generate a svm_rank_train file from the questions
    val (trainingLines:Seq[String], normalizers:Seq[Double]) = questions2svmRankLines(questions, index)

    // Write them to a training file
    val trainingFile = File.createTempFile(UUID.randomUUID.toString, "train")
    FileUtils.writeLines(trainingFile, trainingLines.asJavaCollection)

    // Call svm_rank_train
    val exitCode = s"svm_rank_learn -c 20 -e 0.1  -# 10  ${trainingFile.getCanonicalPath} ${modelFile.getCanonicalPath}".!

    if(exitCode != 0){
      throw new RuntimeException("Error running svm_rank_train!!")
    }
  }

  // "Loads" the model from a file
  def load(file:File, normalizersFile:Option[File] = None){
    this.modelFile = file

    // Load the normalizers, a tsv file with the max values seen on training
    normalizersFile match {
        case Some(f) =>
            val s = Source.fromFile(f).getLines.toList.take(1)
            normalizers = Some(s(0).split('\t').map(_.toDouble))
        case None => Unit
    }
  }

  // Creates a sequence of lines in svm_rank file from a seq of questions
  private def questions2svmRankLines(q:Seq[Question], index:IRIndex, normalizers:Option[Seq[Double]] = None):(Seq[String], Seq[Double]) = {

    val choices:Map[Int, Int] = q.map(question => (question.id -> question.rightChoice.getOrElse(-1))).toMap

    val points = (for(question <- q) yield {

        val choice = question.rightChoice match {
          case Some(i) => i
          case None => 0 // In case this is testing data
        }

        makeDataPoints(question, index)
    }).flatten

    // Normalize the data points
    val maxVals:Seq[Double] = normalizers match {
        case Some(n) => n
        case None => points.map(_.features).transpose.map(_.max)
    }

    val lines = for(point <- points) yield {
      val target = if(point.answerChoice == choices(point.questionId)) 2 else 1

      val sb = new StringBuilder(s"$target qid:${point.questionId}")

      for((feature, j) <- point.features.zipWithIndex){
        // Divide by its max!!
        sb ++= s" ${j+1}:${feature/maxVals(j)}"
      }

      // First element: A sequence of svm_rank strings
      // Second element: The normalizers, one per feature
      sb.toString
    }

    (lines, maxVals)
  }

}

// Entry point to train svm_rank
object TrainIRRanker extends App {

  val config =
      if (args.size < 3) ConfigFactory.load()
      else ConfigFactory.parseFile(new File(args(2))).resolve()

  val indexName = args(0)
  val outFile = new File(args(1))

  val ranker = new IRRanker(config)
  println(s"Training the IR Ranker with ${indexName} and storing the model in ${args(1)}")

  println("Loading training data...")
  val reader = new InputReader(new File(config.getString("trainingFile")))

  println("Loading IR Index...")
  val index = new WikipediaIndex(indexName, config)

  println("Training...")
  ranker.train(reader.questions, index, Some(outFile))

  println("Done.")
}
