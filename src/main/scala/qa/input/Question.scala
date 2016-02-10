package qa.input

// Question - Answers pair. Always assume the first answer is correct if it's
// training data
case class Question(id:String, question:String, choices:Seq[(String, Double)], rightChoice:Option[Int] = None )
