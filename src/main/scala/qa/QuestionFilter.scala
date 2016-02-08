package qa

/**
  * Created by bsharp on 2/5/16.
  */
class QuestionFilter {

  def containsOtherAns (ws:Array[String]):Boolean = {
    val ansLetters = Array("A", "B", "C", "D", "a", "b", "c", "d").toSet
    ws.toSet.intersect(ansLetters).nonEmpty
  }

  def allAbove (s:String): Boolean = {
    s.contains("all of the above")
  }

  def ansChoiceCombos (ws:Array[String]):Boolean = {
    var out = false
    if (ws.contains("both") && ws.contains("and") && containsOtherAns(ws)) out = true
    if (ws.contains("only") && ws.contains("and") && containsOtherAns(ws)) out = true
    out
  }


  def mainFilter (kq: KaggleQuestion): Boolean = {
    if (allAbove(kq.choices.last.text.toLowerCase)) return true
    if (ansChoiceCombos(kq.choices.last.text.toLowerCase.split(" "))) return true
    false
  }

}
