package qa.learning

import java.io.File
import qa.input.Question
import qa.ir._
import com.typesafe.config._

trait Ranker{
    // Call svm_rank or something
    def rerank(list:Seq[Question], index:IRIndex):Seq[Question]

    def makeDataPoints(question:Question, index:IRIndex):Seq[DataPoint] = {
        // Unwind the question object into question/answer pairs to do retrival
        for((choice, ix) <- question.choices.zipWithIndex) yield {
            val queryRes = index.query(question.question, choice.text)
            val features = createFeatureVector(question.question, choice.text, queryRes)
            DataPoint(ix, question.id, question.question, choice.text, features, 0)
        }
    }

    // Extract features from retrived docs
    def createFeatureVector(question:String,
         choice:String, queryRes:QueryResult):Seq[Double]

    // Train the model, save the result to file if provided
    def train(questions:Seq[Question], index:IRIndex,
       outputFile:Option[File] = None, normalizersFile:Option[File] = None):Unit

    // Load an already trained model to this instance
    def load(file:File, normalizers:Option[File] = None):Unit
}

class RankerFactory(config:Config){
    def get(name:String):Ranker = {

        val reranker = name match {
            case "ir" => new IRRanker(config)
            case "translation" => new DummyRanker
            case "neural" => new DummyRanker
            case "dummy" => new DummyRanker
        }

        reranker
    }
}

class DummyRanker extends Ranker{
    def rerank(list:Seq[Question], index:IRIndex):Seq[Question] = list

    def createFeatureVector(question:String,
         choice:String, queryRes:QueryResult):Seq[Double] = Seq()

    def train(questions:Seq[Question],
      index:IRIndex, outputFile:Option[File] = None, normalizersFile:Option[File] = None){}

    def load(file:File, normalizersFile:Option[File] = None){}
}
