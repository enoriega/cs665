package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import edu.smu.tspell.wordnet._
import qa.input._


case class Node(synset: Synset, label: String) {
  var color = Node.WHITE
  override def toString = s"$label: {${synset.getWordForms.mkString(", ")}}"
}

object Node {
  val WHITE = 0
  val GRAY = 1
  val BLACK = 2
}

object GraphUtils {
  val config = ConfigFactory.load()

  System.setProperty(config.getString("wordnet.db_dir_prop"), 
                     config.getString("wordnet.db_dir"))

  val db = WordNetDatabase.getFileInstance

  def getNode(G: Graph[Node, WkLkDiEdge], s: Node): G.NodeT = G.get(s)

  def getNodes(G: Graph[Node, WkLkDiEdge], word: String, 
    tag: String): Queue[G.NodeT] = {
    val pos = tag match {
      case "NN" => SynsetType.NOUN
    }

    val nodes = db.getSynsets(word, pos).foldLeft(Queue[G.NodeT]())(
      (nodes, synset) => {
        val node = getNode(G, Node(synset, tag))
        if(node != null) nodes :+ node
        else nodes
      })

    nodes
  }

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
          
          populateGraph(G, s, "NN")
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
          u.color = Node.BLACK
      }
    }
  }  

  def allPaths(G: Graph[Node, WkLkDiEdge], paths: Queue[Path], p: Path, 
    visited: Set[Node], v: Node, t: Node): Queue[Path] = {
    
    p.push(v)
    visited += v

    if(v == t) {
      println(p.toString)
      paths += p
    }

    else {
      getNode(G, v).outNeighbors.foreach(w => 
        if(!visited.contains(w)) allPaths(G, paths, p, visited, w, t))
    }

    p.pop
    visited -= v

    paths
  }

}