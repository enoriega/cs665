package qa.learning

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
}

object RankerFactory{
    def get(name:String):Ranker = {
        name match {
            case "svm" => new DummyRanker
            case "perceptron" => new DummyRanker
            case "dummy" => new DummyRanker
            case _ => new DummyRanker
        }
    }
}

class DummyRanker extends Ranker{
    def rerank(list:Seq[DataPoint]):Seq[DataPoint] = list

    def createFeatureVector(question:String,
         choice:String, queryRes:QueryResult):Seq[Double] = Seq()
}
