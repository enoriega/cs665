package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable.{Set, Queue, ArrayBuffer}
import qa.input._
import qa.util._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File

object Paths extends App {

  val config = ConfigFactory.load()
  val reader = new InputReader(new File(config.getString("trainingFile")))
  val qs = reader.questions
  
  def wordsToSets(words: Seq[String], G: Graph[Node, WkLkDiEdge]) = {
    words.foldLeft(Set[Set[G.NodeT]]())(
      (nodeSet, word) => nodeSet + GraphUtils.getNodes(G, word, "NN"))
      .foldLeft(Set[G.NodeT]())(
        (nodes, node) => nodes ++ node)
  }


  val qWords = text2set(qs.head.question)
  val aWords = text2set(qs.head.choices(0).text)

  val G = GraphUtils.mkGraph(qWords)
  G ++= GraphUtils.mkGraph(aWords)

  println(G.nodes.foreach(n => println(n.label + " : " + 
    n.synset.getWordForms.mkString(", "))))
    G.edges.foreach(e => println(e._1
    + " --> " + e.label + " --> " +  e._2))

  /*val qWords = Array("chef", "kitchen", "fireplace")
  val G = GraphUtils.mkGraph(qWords)*/

  val qWordSet = wordsToSets(qWords, G)
  val aWordSet = wordsToSets(aWords, G)

  val allPaths = ArrayBuffer[Queue[Path]]()

  qWordSet.foreach(q => {
    aWordSet.foreach(a => allPaths += GraphUtils.genAllPaths(G, q, a))
    })

  //println(allPaths.map(_.filter(_.elems.length < 10)).mkString("\n"))
  GraphUtils.saveTo(G, config.getString("graph.trainingFile"))
}