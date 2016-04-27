package qa.util

import edu.arizona.sista.processors.{Sentence, Document}
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.embeddings.word2vec.Word2Vec
import qa.paths._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable.{Set, ArrayBuffer}
import net.sf.extjwnl.data.POS

object Utils {
  val config = com.typesafe.config.ConfigFactory.load
  val _w2v = new Word2Vec(config.getString("w2v.cwb"), None)

  def scale(value:Double, min:Double, max:Double, 
    lower:Double, upper:Double):Double = {
    if(min == max) return upper
    // the result will be a value in [lower, upper]
    lower + (upper - lower) * (value - min) / (max - min)
  }
}

object w2v extends (String => Array[Double]) {
  def apply(word: String) = Utils._w2v.getWordVector(word) match {
    case Some(vec) => Utils._w2v.norm(vec); vec
    case None => Array.ofDim[Double](Utils._w2v.dimensions)
    }
}

object avgVector extends (Array[Array[Double]] => Array[Double]) {
  def apply(vecs: Array[Array[Double]]) = {
    vecs.transpose.map(_.sum).map(_ * 1.0/vecs.size)
  }
}

object sentence2set extends (Seq[Sentence] => (Array[String], Array[String])) {
  def apply(sentences: Seq[Sentence]) = {

      /*
       * Stop Words
       */
      val nonStopTags   = Array("NN", "VB", "JJ", "RB")
      val stopGeneral   = Array("it", "its", "they", "best", "worst", 
        "everything", "certain", "main" , "s")

      // QA-specific stop words
      val stopQA        = Array("example", "describe", "call", "show", "make",
        "object", "one", "way", "method", "determine", "help")   // function
      val stopQAStarts  = Array("who", "what", "where", "why", "when", 
        "which", "how")

      // Grounded-example stop words
      val stopGrounded  = Array("person", "human")

      // Type/form/kind
      val stopTransparent = Array("kind", "type", "part", "form", "go", 
        "act", "affect")

      // Verbs
      val stopVerbs     = Array("are", "be", "do", "get", "use", "is", "have")

      // Numbers/quantifiers
      val stopQuant     = Array("most", "many", "some", "likely", "usually", 
        "often", "mostly", "small", "large")

      // Other
      val stopOther     = Array("come", "from", "back", "able", 
        "something", "someone")

      // Barron's inference stop words
      val stopBarrons   = Array("other", "say", "also", "consider", "process",
        "thing", "own", "place", "take", "term", "help")

      // Combined list of stop words for QA focus word extraction
      val stopWordsFocus = stopGeneral ++ stopQA ++ stopQAStarts ++ 
        stopGrounded ++ stopTransparent ++ stopVerbs ++ stopQuant ++ 
        stopOther ++ stopBarrons

      /*
       * Stop Phrases
       */
      val stopPhrases = new ArrayBuffer[Array[String]]()

      stopPhrases.append( Array("in", "order", "to") )
      stopPhrases.append( Array("does", "it", "take") )     // e.g. How long does it take ...
      stopPhrases.append( Array("over", "time") )           // e.g. Over time, these decay into ...


    sentences.foldLeft((Array[String](), Array[String]()))(
      (wordSet, sentence) => {
        val tags = sentence.tags.get
        val words = sentence.words
        val lemmas = sentence.lemmas.get
        val nv = ArrayBuffer[String]()
        val tag = ArrayBuffer[String]()
        
        val allTags = Set("NN", "NNS", "JJ", "JJR", "JJS", "VB",
          "VBD", "VBG", "VBN", "VBP", "VBZ")
        val nnTags = Set("NN", "NNS")
        val vbTags = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")
        val jjTags = Set("JJ", "JJR", "JJS")

        for(i <- 0 until tags.length) {
          if(!(stopWordsFocus.contains(lemmas(i)) ||
             stopWordsFocus.contains(words(i)))) {
            if(allTags.contains(tags(i))) {
              if(nnTags.contains(tags(i))) {tag += "NN"; nv += words(i)}
              else if(vbTags.contains(tags(i))) {tag += "VB"; nv += lemmas(i)}
              else if(jjTags.contains(tags(i))) {tag += "JJ"; nv += words(i)}
              }
            }
          }
        (wordSet._1 ++ nv, wordSet._2 ++ tag)
        })
  }
}

object words2set extends ((Seq[String], Seq[String], 
  Graph[Node, WkLkDiEdge]) => Set[Node]) {
  def apply(words: Seq[String], tags: Seq[String], G: Graph[Node, WkLkDiEdge]) = {
    words.zipWithIndex.foldLeft(Set[Set[G.NodeT]]())(
      (nodeSet, wordIndex) => {
        val word = wordIndex._1
        val idx = wordIndex._2
        nodeSet + GraphUtils.getNodes(G, wordIndex._1, tags(idx))
      })
      .foldLeft(Set[Node]())(
        (nodes, node) => nodes ++ node.map(n => Node(n.synset, n.label)))
  }
}

object annotate extends (String => Document) {
  val proc = new FastNLPProcessor
  def apply(text: String) = proc.annotate(text)
}

object text2set extends (String => (Array[String], Array[String])) {
  def apply(text: String) = sentence2set(annotate(text).sentences)
}

object dotProduct extends ((Array[Double], Array[Double]) => Double) {
  def apply(u: Array[Double], v: Array[Double]) = {
    u.zip(v).foldLeft(0.0)((s, vec) => s + vec._1 * vec._2)
  }
}

object plus extends ((Array[Double], Array[Double]) => Array[Double]) {
  def apply(u: Array[Double], v: Array[Double]) = (u, v).zipped.map(_ + _)
}

object rescale extends ((Array[Double], Double, Double) => Array[Double]) {
  def apply(x: Array[Double], lower: Double, upper: Double) = {
    x.map(z => Utils.scale(z, x.min, x.max, lower, upper))
  }
}

object pos extends (String => POS) {
  def apply(tag: String) = tag match {
      case "NN" => POS.NOUN
      case "VB" => POS.VERB
      case "JJ" => POS.ADJECTIVE
    }
}