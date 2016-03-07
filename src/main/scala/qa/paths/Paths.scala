package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable.{Set, Queue}
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import edu.smu.tspell.wordnet._

case class Node(synset: Synset, label: String) {
  var color = Node.WHITE
}
object Node {
  val WHITE = 0
  val GRAY = 1
  val BLACK = 2
}

object Paths extends App {
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  System.setProperty(config.getString("wordnet.db_dir_prop"), 
                     config.getString("wordnet.db_dir"))

  val db = WordNetDatabase.getFileInstance


  def getNode(G: Graph[Node, WkLkDiEdge], s: Node) = G.get(s)

  def nounToSynset(set: Array[NounSynset]) = set.map(s => s.asInstanceOf[Synset])

  /**
   * Returns a mutable graph with pos_tag_annotated synset of words and 
   * key-weighted, key-labeled edges.
   */

  def mkGraph(words: Seq[String]): Graph[Node, WkLkDiEdge] = {
    
    val tags = Array("NN", "VB", "JJ", "RB")
    val nounLinks = Array("hypernym", "hyponym", "instanceHypernym", 
      "instanceHyponym", "memberHolonym", "memberMeronym", "partHolonym",
      "partMeronym", "substanceHolonym", "substanceMeronym")
    val verbLinks = Array("hypernym", "troponym", "verbGroup")
    val adjLinks = Array("similar", "related")
    
    val G: Graph[Node, WkLkDiEdge] = Graph()
    
    // Add NounSynsets
    words.foreach(word => {
      val nounSynsets = db.getSynsets(word, SynsetType.NOUN)
                          .map(x => x.asInstanceOf[NounSynset])
      nounToSynset(nounSynsets).foreach(set => { 
          val s = Node(set, "NN")
          G += s

          val t0 = System.nanoTime()
          populateGraph(G, s, "NN")
          val t1 = System.nanoTime()
          println(s"Elapsed ${(t1-t0)/Math.pow(10,9)} seconds")
      }) 
    })

    // Add VerbSynsets


    // Add AdjectiveSynsets


    // Add AdverbSynsets

    G
  }

  def addEdges(G: Graph[Node, WkLkDiEdge])(
    Q: Queue[G.NodeT],
    u: G.NodeT,
    set: Array[Synset],
    link: String,
    tag: String) = {

    set.foreach(s => {
        val v = Node(s, tag)
        G += ((Node(u.synset, tag) ~%#+#> v)(0, link))
        if(v.color == Node.WHITE) {
          v.color = Node.GRAY
          Q.enqueue(getNode(G, v))
        }
        //println(s"Graph size : ${G.order}, ${G.graphSize}")
    })
  }

  def populateGraph(G: Graph[Node, WkLkDiEdge],
    s: Node,
    tag: String) = {
    
    val Q = Queue[G.NodeT]()
    val sNode = getNode(G, s)
    sNode.color = Node.GRAY
    Q.enqueue(sNode)
    
    while(!Q.isEmpty) {
      val u = Q.dequeue()
      tag match {
        case "NN" =>
          val nouns = u.synset.asInstanceOf[NounSynset]
          addEdges(G)(Q, u, nounToSynset(nouns.getHypernyms), "hypernym", tag)
          addEdges(G)(Q, u, nounToSynset(nouns.getHyponyms), "hyponym", tag)
          u.color = Node.BLACK
      }
    }
  }

  val G = mkGraph(Array("dog"))
  println(G.nodes.foreach(n => println(n.label + " : " + 
    n.synset.getWordForms.mkString(", "))))
  G.edges.foreach(e => println(e._1.synset.getWordForms.mkString(", ") 
    + " --> " + e.label + " --> " +  e._2.synset.getWordForms.mkString(", ")))

}