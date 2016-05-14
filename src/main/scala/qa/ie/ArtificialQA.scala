package qa.ie

case class ArtificialQA(val original:String, val qtype:String, val question:String, val answer:String, val questionNouns:Seq[String], val answerNouns:Seq[String], val alternatives:Seq[String] = Seq(), val justification:String = ""){
  override def equals(o: Any) = o match {
    case that: ArtificialQA => this.answer.toLowerCase == that.answer.toLowerCase
    case _ => false
  }

  override def hashCode = {
    val s = this.answer.toLowerCase
    s.hashCode
  }
}
class NoQA extends ArtificialQA("", "","","", Seq(), Seq())
