package qa.voting

import qa.input.Question

/**
  * Created by bsharp on 2/5/16.
  */
class QuestionFilter {

  def containsOtherAns (ws:Seq[String]):Boolean = {
    val ansLetters = Seq("A", "B", "C", "D", "a", "b", "c", "d").toSet
    ws.toSet.intersect(ansLetters).nonEmpty
  }

  def allAbove (s:String): Boolean = {
    s.contains("all of the above")
  }

  def ansChoiceCombos (ws:Seq[String]):Boolean = {
    var out = false
    if (ws.contains("both") && ws.contains("and") && containsOtherAns(ws)) out = true
    if (ws.contains("only") && ws.contains("and") && containsOtherAns(ws)) out = true
    out
  }


  def mainFilter (kq: Question): Boolean = {
    if (allAbove(kq.choices.last.text.toLowerCase)) return true
    if (ansChoiceCombos(kq.choices.last.text.toLowerCase.split(" "))) return true
    false
  }

}
