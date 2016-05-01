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
  
  val (qWords, qTags) = text2set(qs.head.question)
  val (aWords, aTags) = text2set(qs.head.choices(0).text)

  val G = GraphUtils.mkGraph(qWords, qTags)
  G ++= GraphUtils.mkGraph(aWords, aTags)

  println(G.nodes.foreach(n => println(n.label + " : " + n.toString)))
    G.edges.foreach(e => println(e._1
    + " --> " + e.label + " --> " +  e._2))

  /*val qWords = Array("chef", "kitchen", "fireplace")
  val G = GraphUtils.mkGraph(qWords)*/

  val qWordSet = words2set(qWords, qTags, G)
  val aWordSet = words2set(aWords, aTags, G)

  val allPaths = ArrayBuffer[Queue[Path]]()

  qWordSet.foreach(q => {
    aWordSet.foreach(a => allPaths += GraphUtils.genAllPaths(G, q, a))
    })

  println(allPaths.map(_.filter(_.elems.length < 10)).mkString("\n"))
  //GraphUtils.saveTo(G, config.getString("graph.trainingFile"))
}