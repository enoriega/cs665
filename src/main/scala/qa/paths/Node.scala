package qa.paths

import edu.smu.tspell.wordnet._

case class Node(synset: Synset, label: String) {
  var color = Node.WHITE
  override def toString = s"$label: {${synset.getWordForms.mkString(", ")}}"
}

object Node {
  val WHITE = 0
  val GRAY = 1
  val BLACK = 2
}

