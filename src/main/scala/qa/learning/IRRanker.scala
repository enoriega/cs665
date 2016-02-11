package qa.learning

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

  // Runs svm_classify and resorts the list with the new order
  def rerank(questions:Seq[Question], index:IRIndex):Seq[Question] = {
    // Load the corresponding model depending on the index's name
    val modelFile = new File(config.getString(s"indexes.${index.name}.irModelFile"))
    this.load(modelFile)

    val lines:Seq[String] = questions2svmRankLines(questions, index)

    // Write them to a training file
    val classificationFile = File.createTempFile(UUID.randomUUID.toString, "rank")
    FileUtils.writeLines(classificationFile, lines.asJavaCollection)

    val outputFile = File.createTempFile(UUID.randomUUID.toString, "txt")

    // Call svm_rank_train
    val exitCode = s"svm_rank_classify ${classificationFile.getCanonicalPath} ${this.modelFile.getCanonicalPath} ${outputFile.getCanonicalPath}".!

    if(exitCode != 0){
      throw new RuntimeException("Error running svm_rank_train!!")
    }

    // Fetch the output file and select a choice per question
    val results = io.Source.fromFile(outputFile).getLines.map(_.toDouble).grouped(4).toList

    for((q, r) <- questions zip results) yield {
      val prediction = r.indexOf(r.max)
      // Plug svm_rank numerical score into the choices of the question object
      val newChoices = q.choices.zip(r).map{case (choice, score) => (choice._1, score)}
      Question(q.id, q.question, newChoices, Some(prediction))
    }
  }

  // Generates features out of the IR query
  def createFeatureVector(question:String,
       choice:String, queryRes:QueryResult):Seq[Double] = {
         val sum = queryRes.topDocs.map(_.score).sum
         Seq(queryRes.numResults, queryRes.maxScore, sum)
  }

  // Trains svm_rank with this questions given this index
  def train(questions:Seq[Question], index:IRIndex, outputFile:Option[File] = None){
    // Writes an svm_rank training file
    modelFile = outputFile match {
      case Some(f) => f
      case None => File.createTempFile(UUID.randomUUID.toString, "model")
    }

    // Generate a svm_rank_train file from the questions
    val trainingLines:Seq[String] = questions2svmRankLines(questions, index)

    // Write them to a training file
    val trainingFile = File.createTempFile(UUID.randomUUID.toString, "train")
    FileUtils.writeLines(trainingFile, trainingLines.asJavaCollection)

    // Call svm_rank_train
    val exitCode = s"svm_rank_learn -c 1 -# 10  ${trainingFile.getCanonicalPath} ${modelFile.getCanonicalPath}".!

    if(exitCode != 0){
      throw new RuntimeException("Error running svm_rank_train!!")
    }
  }

  // "Loads" the model from a file
  def load(file:File){
    this.modelFile = file
  }

  // Creates a sequence of lines in svm_rank file from a seq of questions
  private def questions2svmRankLines(q:Seq[Question], index:IRIndex) = {
    val ret = for((question, id) <- q.zipWithIndex) yield {

        val choice = question.rightChoice match {
          case Some(i) => i
          case None => 0 // In case this is testing data
        }

        val points = makeDataPoint(question, index)

        for(point <- points) yield {
          val target = if(point.answerChoice == choice) 2 else 1

          val sb = new StringBuilder(s"$target qid:$id")

          for((feature, j) <- point.features.zipWithIndex){
            sb ++= s" ${j+1}:$feature"
          }

          sb.toString
        }
    }
    ret.flatten
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
