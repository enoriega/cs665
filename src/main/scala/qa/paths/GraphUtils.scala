package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.io.dot._
import implicits._
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.predefined.WkLkDi
import scala.collection.mutable._
import scala.collection.JavaConverters._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.{BufferedWriter, FileWriter, File, IOException}
import java.util.NoSuchElementException
import edu.arizona.sista.embeddings.word2vec.Word2Vec
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.Sentence
import qa.input._
import qa.util._
import net.liftweb.json.{JsonAST, JsonParser, Printer}
import net.sf.extjwnl.dictionary.Dictionary
import net.sf.extjwnl.data.POS
import net.sf.extjwnl.data.Word
import net.sf.extjwnl.data.Synset
import net.sf.extjwnl.data.PointerUtils

object GraphUtils {
  val config = ConfigFactory.load()
  val dict = Dictionary.getDefaultResourceInstance
  val proc = new FastNLPProcessor()

  def getNode(G: Graph[Node, WkLkDiEdge], s: Node): Option[G.NodeT] = {
    try {
      Some(G get s)
    } catch {
      case e: NoSuchElementException => None
    }
  }

  def getNodes(word: String, tag: String) = {
    val iw = dict.getIndexWord(pos(tag), word)
    val nodes = iw match {
      case null => Set[Node]()
      case _ => iw.getSenses.asScala.toArray.map(Node(_, tag)).toSet
    }
    nodes
  }

  def getNodes(G: Graph[Node, WkLkDiEdge], word: String, 
    tag: String): Set[G.NodeT] = {

    val iw = dict.getIndexWord(pos(tag), word)
    val nodes = iw match {
      case null => Set[G.NodeT]()
      case _ => iw.getSenses.asScala.toArray.foldLeft(Set[G.NodeT]())(
      (nodes, synset) => {
        val node = getNode(G, Node(synset, tag))
        node match {
          case Some(node) => nodes + node
          case None => nodes
        }
      })
    }

    nodes
  }

  def topKSynsets(ref: Seq[String], sets: Array[Synset], k: Int) = {
    sets.map(s => {
      val tmp = Node(s, "")
      val (dw, dt) = text2set(tmp.defn)
      val defn = dw.zipWithIndex.filter({
        case (w, i) => dt(i).equals("NN")
        }).toMap.keys.toArray
       (s, Utils._w2v.sanitizedTextSimilarity(ref.toList, defn.toList))
      }).sortWith(_._2 > _._2).slice(0, k).map(_._1)
  }

  /**
   * Returns a mutable graph with pos_tag_annotated synset of words and 
   * key-weighted, key-labeled edges.
   */

  def mkGraph(words: Seq[String], tags: Seq[String]): Graph[Node, WkLkDiEdge] = {
    
    val nounLinks = Array("hypernym", "hyponym", "instanceHypernym", 
      "instanceHyponym", "memberHolonym", "memberMeronym", "partHolonym",
      "partMeronym", "substanceHolonym", "substanceMeronym")
    val verbLinks = Array("hypernym", "troponym", "verbGroup")
    val adjLinks = Array("similar", "related")
    
    val G: Graph[Node, WkLkDiEdge] = Graph()
    
    // Add NounSynsets
    words.zip(tags).foreach(
      { case (word, tag) => {
          val iw = dict.getIndexWord(pos(tag), word)
          val posSynsets = iw match {
            case null => Array[Synset]()
            case _ => iw.getSenses.asScala.toArray
            } 
          val topKsets = topKSynsets(words, posSynsets, 1)
          topKsets.foreach(set => { 
            val s = Node(set, tag)
            G += s
            populateGraph(G, s, tag, words)
          })
      } 
    })

    G
  }

  def addEdges(G: Graph[Node, WkLkDiEdge])(
    Q: Queue[G.NodeT],
    u: G.NodeT,
    set: Array[Synset],
    link: String,
    tag: String) = {

    set.foreach(s => {
      val v = getNode(G, Node(s, tag)) match {
        case Some(node) => val _node = Node(node.synset, node.label)
          _node.color = node.color
          _node
        case None => Node(s, tag)
      }
      
      G += ((Node(u.synset, tag) ~%#+#> v)(0, link))
      if(link equals "hypernym")
        G += ((v ~%#+#> (Node(u.synset, tag)))(0, "hyponym"))
      else if(link equals "holonym")
        G += ((v ~%#+#> (Node(u.synset, tag)))(0, "meronym"))
      else if(link equals "similar")
        G += ((v ~%#+#> (Node(u.synset, tag)))(0, "similar"))

      if(v.color == Node.WHITE) {
        v.color = Node.GRAY
        Q.enqueue(getNode(G, v).get)
      }
      //println(s"Graph size : ${G.order}, ${G.graphSize}")
    })
  }

  def populateGraph(G: Graph[Node, WkLkDiEdge],
    s: Node,
    tag: String,
    words: Seq[String]) = {
    
    val Q = Queue[G.NodeT]()
    val sNode = getNode(G, s).get
    sNode.color = Node.GRAY
    Q.enqueue(sNode)
    
    while(!Q.isEmpty) {
      val u = Q.dequeue()
      val set = u.synset
      tag match {
        case "NN" =>
          val hypernyms = PointerUtils.getDirectHypernyms(set).asScala.toArray.map(_.getSynset)
          val holonyms = PointerUtils.getPartHolonyms(set).asScala.toArray.map(_.getSynset)
          val similar = PointerUtils.getSynonyms(set).asScala.toArray.map(_.getSynset)
          addEdges(G)(Q, u, topKSynsets(words, hypernyms, 1), "hypernym", tag)
          addEdges(G)(Q, u, topKSynsets(words, holonyms, 1), "holonym", tag)
          addEdges(G)(Q, u, topKSynsets(words, similar, 1), "similar", tag)
          u.color = Node.BLACK
        case "VB" =>
          val hypernyms = PointerUtils.getDirectHypernyms(set).asScala.toArray.map(_.getSynset)
          val similar = PointerUtils.getSynonyms(set).asScala.toArray.map(_.getSynset)
          addEdges(G)(Q, u, topKSynsets(words, hypernyms, 1), "hypernym", tag)
          addEdges(G)(Q, u, topKSynsets(words, similar, 1), "similar", tag)
          u.color = Node.BLACK
        case "JJ" =>
          val similar = PointerUtils.getSynonyms(set).asScala.toArray.map(_.getSynset)
          addEdges(G)(Q, u, topKSynsets(words, similar, 1), "similar", tag)
      }
    }
  }  

  def genAllPaths(G: Graph[Node, WkLkDiEdge], v: Node, t: Node) = {
    val paths = Queue[Path]()
    def genAllPathsHelper(G: Graph[Node, WkLkDiEdge], p: Path,
      visited: Set[Node], v: Node, t: Node): Unit = {
      
      val u = p.top
      
      u match {
        case Some(u) => val uE = getNode(G,v).get.edges.find(_._1.equals(u)).get
          p(0) = (u, Some((uE.label.toString, uE.weight)))
        case None =>
      }

      p.push(v, None)
      visited += v

      if(v == t) paths += new Path().copy(p.elems)
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

  object PositionedNodeDescriptor {
    import net.liftweb.json._
    final class NodeSerializer extends CustomSerializer[Node] ( fmts => (
      { case JArray(JString(label) :: JString(offset) :: Nil) => 
          Node(dict.getSynsetAt(pos(label), offset.toLong), label)
        },
      {
        case Node(set, label) => JArray(JString(label) :: 
          JString(set.getOffset.toString) :: Nil)
        }
      ))
    val node = new NodeDescriptor[Node](
      typeId = "Nodes",
      customSerializers = Seq(new NodeSerializer)) {
      def id(node: Any) = node match {
        case n@Node(s, l) => s.getOffset.toString
        }
      }
    }

  val descriptor = new Descriptor[Node](
    defaultNodeDescriptor = PositionedNodeDescriptor.node,
    defaultEdgeDescriptor = WkLkDi.descriptor[Node, String](""),
    namedNodeDescriptors  = Seq(PositionedNodeDescriptor.node),
    namedEdgeDescriptors  = Seq(WkLkDi.descriptor[Node, String](""))
    )


  def saveTo(G: Graph[Node, WkLkDiEdge], file: String) = {
    val exported = G.toJson(descriptor)
    val pretty = Printer.pretty(JsonAST.render(JsonParser.parse(exported)))

    val bw = new BufferedWriter(new FileWriter(file))
    try {
      bw.write(pretty)
    } catch {
      case ioe: IOException => println("Exception!")
    }

    bw.flush()
    bw.close()
  }

  def load(jsonFile: String) = {
    val source = scala.io.Source.fromFile(jsonFile)
    val lines = try source.getLines mkString "\n" finally source.close()
    Graph.fromJson[Node, WkLkDiEdge](lines, descriptor)
  }

  def toDot(G: Graph[Node, WkLkDiEdge], dotFile: String, 
    exec: Boolean = false) = {
    import sys.process._
    val root = DotRootGraph(directed = true, id = Some("WordNet_Graph"))

    def edgeTransformer(ie: scalax.collection.Graph[Node, WkLkDiEdge]#EdgeT):
      Option[(DotGraph, DotEdgeStmt)] = ie.edge match {
      case WkLkDiEdge(src, target, weight, label) => label match {
      case label: String =>
        Some((root, DotEdgeStmt(src.toString, target.toString, 
          List(DotAttr("label",label.toString)))))
      }
    }

    val _G = G.asInstanceOf[scalax.collection.Graph[Node,WkLkDiEdge]]
    val dot = _G.toDot(root, edgeTransformer)
    val bw = new BufferedWriter(new FileWriter(dotFile))
    try {
      bw.write(dot)
    } catch {
      case ioe: IOException => println("Exception!")
    }

    bw.flush()
    bw.close()

    if(exec) s"dot -Tpng ${dotFile} -o graph.png".!
    Unit
  }
}