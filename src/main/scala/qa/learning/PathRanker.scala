package qa.learning

import com.typesafe.config.ConfigFactory
import qa.input._
import qa.ir._
import qa.paths._
import qa.util._
import qa.qtype.BottomUpClassify
import com.typesafe.config.{ConfigFactory, Config}
import java.io.File
import edu.arizona.sista.learning.{SVMRankingClassifier, RVFRankingDataset}
import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._

class PathRanker extends Ranker {
  val svmRanker = new SVMRankingClassifier[String](".")
  val dataset = new RVFRankingDataset[String]
  var G: Graph[Node, WkLkDiEdge] = Graph()
  
  def rerank(questions:Seq[Question], index:IRIndex):Seq[Question] = Seq()

  def mkFeatures(q: Question, ans: Answer) = {
    val words = sentence2set(q.annotation.get.sentences)
    val counter = new Counter[String]
    words.map(w => counter.incrementCount(w, 1))
    counter
  }

  def createFeatureVector(question:String,
    choice:String, queryRes:QueryResult):Seq[Double] = Seq()

  def train(questions:Seq[Question],
    index:IRIndex, outputFile:Option[File] = None, 
    normalizersFile:Option[File] = None) = {

    val t_2 = System.nanoTime
    BottomUpClassify.annotateQuestions(questions)
    val t_1 = System.nanoTime
    println(s"Checkpoint #1: ${(t_1-t_2)/1.0*Math.pow(10,9)} seconds")
    
    // Init graph
    val t0 = System.nanoTime
    var done = 0
    val total = questions.size * 5
    questions.foreach(q => {
      G = GraphUtils.mkGraph(sentence2set(q.annotation.get.sentences))
      q.choices.foreach(ans => {
        G ++= GraphUtils.mkGraph(sentence2set(ans.annotation.get.sentences))
        })
      done += 5
      print(s"$done/$total added to graph\r")
      })
    val t1 = System.nanoTime
    println(s"\nCheckpoint #2: ${(t1-t0)/1.0*Math.pow(10,9)} seconds")

    GraphUtils.saveTo(G, ConfigFactory.load.getString("graph.trainingFile"))

    val t2 = System.nanoTime
    questions.foreach(q => {
      dataset += q.choices.zipWithIndex.reverse.foldLeft(
        List[Datum[Int, String]]())(
          (answers, choice) => 
            new RVFDatum[Int, String](choice._2, 
              mkFeatures(q, choice._1)) :: answers)
      })
    val t3 = System.nanoTime
    println(s"Checkpoint #3: ${(t3-t2)/1.0*Math.pow(10,9)} seconds")

    svmRanker.train(dataset)
  }

  def load(file:File, normalizersFile:Option[File] = None) = {
    G = GraphUtils.load(file.getPath)
  }
} 