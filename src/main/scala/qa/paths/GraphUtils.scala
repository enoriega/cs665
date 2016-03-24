package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import java.util.NoSuchElementException
import edu.smu.tspell.wordnet._
import qa.input._

object GraphUtils {
  val config = ConfigFactory.load()

  System.setProperty(config.getString("wordnet.db_dir_prop"), 
                     config.getString("wordnet.db_dir"))

  val db = WordNetDatabase.getFileInstance

  def getNode(G: Graph[Node, WkLkDiEdge], s: Node): Option[G.NodeT] = {
    try {
      Some(G get s)
    } catch {
      case e: NoSuchElementException => None
    }
  }

  def getNodes(G: Graph[Node, WkLkDiEdge], word: String, 
    tag: String): Set[G.NodeT] = {
    val pos = tag match {
      case "NN" => SynsetType.NOUN
    }

    val nodes = db.getSynsets(word, pos).foldLeft(Set[G.NodeT]())(
      (nodes, synset) => {
        val node = getNode(G, Node(synset, tag))
        node match {
          case Some(node) => nodes + node
          case None => nodes
        }
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
        if(link equals "hypernym")
          G += ((v ~%#+#> (Node(u.synset, tag)))(0, "hyponym"))
        if(v.color == Node.WHITE) {
          v.color = Node.GRAY
          Q.enqueue(getNode(G, v).get)
        }
        //println(s"Graph size : ${G.order}, ${G.graphSize}")
    })
  }

  def populateGraph(G: Graph[Node, WkLkDiEdge],
    s: Node,
    tag: String) = {
    
    val Q = Queue[G.NodeT]()
    val sNode = getNode(G, s).get
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

  def genAllPaths(G: Graph[Node, WkLkDiEdge], v: Node, t: Node) = {
    val paths = Queue[Path]()
    def genAllPathsHelper(G: Graph[Node, WkLkDiEdge], p: Path,
      visited: Set[Node], v: Node, t: Node): Unit = {
      
      p.push(v)
      visited += v

      if(v == t) paths += new Path().copy(p.nodes)
      else {
        getNode(G, v).get.outNeighbors.foreach(w => 
          if(!visited.contains(w)) genAllPathsHelper(G, p, visited, w, t))
      }

      p.pop
      visited -= v
    }

    genAllPathsHelper(G, new Path, Set[Node](), v, t)
    paths
  }
}