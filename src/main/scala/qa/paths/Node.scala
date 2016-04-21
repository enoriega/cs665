package qa.paths

import edu.smu.tspell.wordnet._
import qa.util._

case class Node(synset: Synset, label: String) {
  var color = Node.WHITE
  override def toString = s"$label: {${synset.getWordForms.mkString(", ")}}"
  val set2v = avgVector(synset.getWordForms.foldLeft(Array[String]())(
    (words, word) => words ++ word.split("\\s+")).map(w2v(_)))
  val def2v = avgVector(text2set(synset.getDefinition).toArray.map(w2v(_)))
}

object Node {
  val WHITE = 0
  val GRAY = 1
  val BLACK = 2
}

