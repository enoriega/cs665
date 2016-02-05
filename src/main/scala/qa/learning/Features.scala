package qa.learning

import qa.input.Question
import qa.ir._

// Creates DataPoint instances, here is where the magic happens.
object FeatureExtractor{
    def makeDataPoint(question:Question, index:IRIndex):Seq[DataPoint] = {
        // Unwind the question object into question/answer pairs to do retrival
        for((choice, ix) <- question.choices.zipWithIndex) yield {
            val documents = index.query(question.question, choice)
            val features = createFeatureVector(question.question, choice, documents)
            DataPoint(ix, question.question, choice, features, 0)
        }
    }

    // Extract features from retrived docs
    def createFeatureVector(question:String,
         choice:String, documents:Seq[Document]):Seq[Double] = {

        // Don't forget to normalize the features
        Seq()
    }
}
