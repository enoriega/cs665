package qa.learning

// Represents a feature vector and it´s original question answer pair
case class DataPoint(answerChoice:Int, questionId:Int, question:String,
     answer:String, features:Seq[Double], rankingConfidence:Double){

}
