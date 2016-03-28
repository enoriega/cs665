package qa.input

import edu.arizona.sista.processors.Document

// Question - Answers pair. Always assume the first answer is correct if it's
// training data
case class Question(id:Int, question:String, choices:Seq[Answer], rightChoice:Option[Int] = None, var annotation:Option[Document] = None) {
  override def toString():String = s"${question}\t${rightChoice.getOrElse(-1)}\t${choices.map(c => c.text).mkString("\t")}"
}

case class Answer (text:String, score:Double, var annotation:Option[Document] = None)
