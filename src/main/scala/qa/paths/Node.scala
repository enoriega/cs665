package qa.paths


import net.sf.extjwnl.data.Synset
import qa.util._
import scala.collection.JavaConverters._

case class Node(synset: Synset, label: String) {
  var color = Node.WHITE
  override def toString = synset.getWords.asScala.toArray.map(_.getLemma).mkString(", ")
  val set2v = avgVector(synset.getWords.asScala.toArray.map(_.getLemma)
    .foldLeft(Array[String]())((words, word) => 
      words ++ word.split("\\s+")).map(w2v(_)))
  val gloss = synset.getGloss
  val defn = if(gloss.indexOf(";") == -1) gloss 
             else gloss.substring(0, gloss.indexOf(";"))
  val def2v = avgVector(text2set(defn)._1
    .toArray.map(w2v(_)))
}

object Node {
  val WHITE = 0
  val GRAY = 1
  val BLACK = 2
}

