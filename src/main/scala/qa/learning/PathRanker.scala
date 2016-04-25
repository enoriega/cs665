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
import scala.collection.mutable.{ArrayBuffer, Queue}
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._

class PathRanker extends Ranker {
  val svmRanker = new SVMRankingClassifier[String](".")
  val dataset = new RVFRankingDataset[String]
  var G: Graph[Node, WkLkDiEdge] = Graph()
  
  def rerank(questions:Seq[Question], index:IRIndex):Seq[Question] = Seq()

  def S(q: Question, a: Answer, p: Path): (Array[Node],Array[Double]) = {
    val q2v = avgVector(sentence2set(q.annotation.get.sentences)._1
      .toArray.map(w2v(_)))
    val a2v = avgVector(sentence2set(a.annotation.get.sentences)._1
      .toArray.map(w2v(_)))

    val scores = p.elems.reverse.toArray.map(_._1).map(node =>
      (node, dotProduct(plus(q2v,a2v), plus(node.def2v, node.set2v))))
    val normalise = scores.map(_._2)
    Utils._w2v.norm(normalise)
    (scores.map(_._1), normalise)
  }

  def mkFeatures(q: Question, a: Answer) = {
    val (qWords, qTags) = sentence2set(q.annotation.get.sentences)
    val (aWords, aTags) = sentence2set(a.annotation.get.sentences)
    
    val qWordSet = words2set(qWords, qTags, G)
    val aWordSet = words2set(aWords, aTags, G)

    val _paths = ArrayBuffer[Queue[Path]]()

    qWordSet.foreach(q => {
      aWordSet.foreach(a => _paths += GraphUtils.genAllPaths(G, q, a))
    })

    val allPaths = _paths.foldLeft(Array[Path]())(
      (queue, paths) => queue ++ paths)

    val ns = allPaths.map(S(q, a, _))
    val (nodes, scores) = (ns.map(_._1), ns.map(_._2))
    val summedScores = scores.map(_.sum)
    Utils._w2v.norm(summedScores)

    val numPaths = allPaths.size
    val minPathL = allPaths.foldLeft(Int.MaxValue)((len, p) => 
      if(p.elems.size < len) p.elems.size else len)
    val avgPathL = allPaths.foldLeft(0)(_ + _.elems.size)/allPaths.size

    val maxScore = summedScores.max
    val avgScore = summedScores.foldLeft(0.0)(_ + _)/summedScores.size

    println(nodes(summedScores.indexOf(maxScore)).toString)

    val counter = new Counter[String]
    counter.incrementCount("num_paths", numPaths)
    counter.incrementCount("min_path_len", minPathL)
    counter.incrementCount("avg_path_len", avgPathL)
    counter.incrementCount("max_score", maxScore)
    counter.incrementCount("avg_score", avgScore)
    counter
  }

  def createFeatureVector(question:String,
    choice:String, queryRes:QueryResult):Seq[Double] = Seq()

  def train(questions:Seq[Question],
    index:IRIndex, outputFile:Option[File] = None, 
    normalizersFile:Option[File] = None) = {

    BottomUpClassify.annotateQuestions(questions)
    
    // Init graph
    var done = 0
    val total = questions.size

    questions.foreach(q => {
      val (qWords, qTags) = sentence2set(q.annotation.get.sentences)
      G = GraphUtils.mkGraph(qWords, qTags)
      q.choices.foreach(ans => {
        val (aWords, aTags) = sentence2set(ans.annotation.get.sentences)
        G ++= GraphUtils.mkGraph(aWords, aTags)
        })
      GraphUtils.saveTo(G, s"${ConfigFactory.load.getString("graph.folder")}/${q.id}.json")
      done += 1
      print(s"$done/$total Qs added to graph\r")
      /*dataset += q.choices.zipWithIndex.reverse.foldLeft(
        List[Datum[Int, String]]())(
          (answers, choice) => 
            new RVFDatum[Int, String](choice._2, 
              mkFeatures(q, choice._1)) :: answers)*/
      })
      print("\n")
    //svmRanker.train(dataset)
  }

  def load(file:File, normalizersFile:Option[File] = None) = {
    G = GraphUtils.load(file.getPath)
  }
} 