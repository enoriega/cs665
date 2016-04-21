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

object Utils

object w2v extends (String => Array[Double]) {
  val config = com.typesafe.config.ConfigFactory.load
  val _w2v = new Word2Vec(config.getString("w2v.cwb"), None)
  def apply(word: String) = _w2v.getWordVector(word) match {
    case Some(vec) => vec
    case None => Array.ofDim[Double](_w2v.dimensions)
    }
  
}

object avgVector extends (Array[Array[Double]] => Array[Double]) {
  def apply(vecs: Array[Array[Double]]) = {
    vecs.transpose.map(_.sum).map(_ * 1.0/vecs.size)
  }
}

object sentence2set extends (Seq[Sentence] => Array[String]) {
  def apply(sentences: Seq[Sentence]) = {
    sentences.foldLeft(Array[String]())(
      (wordSet, sentence) => {
        val tags = sentence.tags.get
        val words = sentence.words
        val nv = ArrayBuffer[String]()
        
        for(i <- 0 until tags.length)
          if(tags(i).matches("NN(.*)"))// || tags(i).matches("VB(.*)")) 
            nv += words(i)

        wordSet ++ nv
        })
  }
}

object annotate extends (String => Document) {
  val proc = new FastNLPProcessor
  def apply(text: String) = proc.annotate(text)
}

object text2set extends (String => Seq[String]) {
  def apply(text: String) = sentence2set(annotate(text).sentences)
}