package qa.paths

import scala.collection.mutable._
import qa.input._
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.Sentence

class Path {
  val nodes = Stack[Node]()

  override def toString = nodes.reverse.mkString(" -> ")

  def push(node: Node) = { nodes.push(node); this }
  def pop() = nodes.pop()
}

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
    + " --> " + e.label + " --> " +  e._2.synset.getWordForms.mkString(", ")))*/

  val qNodeList = qWords.foldLeft(Array[Queue[G.NodeT]]())(
    (nodes, word) => nodes :+ GraphUtils.getNodes(G, word, "NN"))
  val nodeSet0 = qNodeList(0)
  val node = nodeSet0(0)
  val finalNode = GraphUtils.getNodes(G, "entity", "NN")(0)

  val paths = GraphUtils.allPaths(G, Queue[Path](), new Path, 
    Set[Node](), node, finalNode)
  
  println(paths.size)
}