package qa.ie

case class ArtificialQA(val qtype:String, val question:String, val answer:String, val alternatives:Seq[String] = Seq(), val justification:String = "")
class NoQA extends ArtificialQA("","","")
