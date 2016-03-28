package qa.ie

case class ArtificialQA(val qtype:String, val question:String, val answer:String, val justification:String)
class NoQA extends ArtificialQA("","","","")
