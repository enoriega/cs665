package qa.input

// Question - Answers pair. Always assume the first answer is correct if it's
// training data
case class Question(id:Int, question:String, choices:Seq[Answer], rightChoice:Option[Int] = None )

case class Answer (val text:String, val score:Double)
