package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable._
import qa.input._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.Sentence

object Paths extends App {

  val config = ConfigFactory.load()
  val reader = new InputReader(new File(config.getString("trainingFile")))
  val qs = reader.questions
  
  def wordSet(sentences: Seq[Sentence]) = {
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

  def wordsToSets(words: Seq[String], G: Graph[Node, WkLkDiEdge]) = {
    words.foldLeft(Set[Set[G.NodeT]]())(
      (nodeSet, word) => nodeSet + GraphUtils.getNodes(G, word, "NN"))
      .foldLeft(Set[G.NodeT]())(
        (nodes, node) => nodes ++ node)
  }


  val proc = new FastNLPProcessor()
  var doc = proc.annotate(qs.head.question)
  val qWords = wordSet(doc.sentences)
  
  doc = proc.annotate(qs.head.choices(0).text)
  val aWords = wordSet(doc.sentences)

  val G = GraphUtils.mkGraph(qWords)
  G ++= GraphUtils.mkGraph(aWords)

  /*println(G.nodes.foreach(n => println(n.label + " : " + 
    n.synset.getWordForms.mkString(", "))))
    G.edges.foreach(e => println(e._1.synset.getWordForms.mkString(", ") 
    + " --> " + e.label + " --> " +  e._2.synset.getWordForms.mkString(", ")))
    */

  /*val qWords = Array("chef", "kitchen", "fireplace")
  val G = GraphUtils.mkGraph(qWords)*/

  val qWordSet = wordsToSets(qWords, G)
  val aWordSet = wordsToSets(aWords, G)

  val allPaths = ArrayBuffer[Queue[Path]]()

  qWordSet.foreach(q => {
    aWordSet.foreach(a => allPaths += GraphUtils.genAllPaths(G, q, a))
    })
}