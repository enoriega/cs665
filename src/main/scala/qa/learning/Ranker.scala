package qa.learning

import java.io.File
import qa.input.Question
import qa.ir._

trait Ranker{
    // Call svm_rank or something
    def rerank(list:Seq[DataPoint]):Seq[DataPoint]

    def makeDataPoint(question:Question, index:IRIndex):Seq[DataPoint] = {
        // Unwind the question object into question/answer pairs to do retrival
        for((choice, ix) <- question.choices.zipWithIndex) yield {
            val queryRes = index.query(question.question, choice)
            val features = createFeatureVector(question.question, choice, queryRes)
            DataPoint(ix, question.question, choice, features, 0)
        }
    }

    // Extract features from retrived docs
    def createFeatureVector(question:String,
         choice:String, queryRes:QueryResult):Seq[Double]

    // Train the model, save the result to file if provided
    def train(questions:Seq[Question], index:IRIndex,
       outputFile:Option[File] = None):Unit

    // Load an already trained model to this instance
    def load(file:File):Unit
}

object RankerFactory{
    def get(name:String, modelFile:Option[File] = None):Ranker = {

        val reranker = name match {
            case "ir" => new DummyRanker
            case "translation" => new DummyRanker
            case "neural" => new DummyRanker
            case "dummy" => new DummyRanker
        }
        modelFile match {
          case Some(file) => reranker.load(file)
          case _ => Unit
        }

        reranker
    }
}

class DummyRanker extends Ranker{
    def rerank(list:Seq[DataPoint]):Seq[DataPoint] = list

    def createFeatureVector(question:String,
         choice:String, queryRes:QueryResult):Seq[Double] = Seq()

    def train(questions:Seq[Question],
      index:IRIndex, outputFile:Option[File] = None){}

    def load(file:File){}
}
