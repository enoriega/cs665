package qa.learning

import com.typesafe.config.ConfigFactory
import qa.input._
import qa.ir._
import qa.paths._
import qa.util._
import qa.qtype.BottomUpClassify
import com.typesafe.config.{ConfigFactory, Config}
import java.io._
import java.util.concurrent.CyclicBarrier
import edu.arizona.sista.learning.{SVMRankingClassifier, RVFRankingDataset}
import edu.arizona.sista.learning.{Datum, RVFDatum, ScaleRange, Datasets}
import edu.arizona.sista.struct._
import scala.collection.mutable.{ArrayBuffer, Queue}
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._

class PathRanker(c: Double = 0.1, keepFiles: Boolean = false) 
  extends Ranker {
  val config = ConfigFactory.load
  val svmRanker = new SVMRankingClassifier[String](config.getString("graph.folder"), 
    cLight = c, keepIntermediateFiles = keepFiles)
  val dataset = new RVFRankingDataset[String]
  var numThreads: Option[Int] = None
  var barrier: Option[CyclicBarrier] = None
  var scaleRange: Option[ScaleRange[String]] = None
  
  def rerank(questions:Seq[Question], index:IRIndex):Seq[Question] = {
    BottomUpClassify.annotateQuestions(questions)

    var done = 0
    val total = questions.size

    val scores = ArrayBuffer[Array[Double]]()
    questions.foreach(q => {
      val G = Some(GraphUtils.load(s"${config.getString("graph.folder")}/${q.id}.json"))
      val qid = q.choices.zipWithIndex.reverse.foldLeft(
        List[Datum[Int, String]]())(
          (answers, choice) => {
            val features = mkFeatures(G, q, choice._1)
            val scaledFeatures = Datasets.svmScaleDatum[String](features,
              scaleRange.get, 0.0, 1.0)
            new RVFDatum[Int, String](0, scaledFeatures) :: answers
          })
      done += 1
      print(s"$done/$total added to G\r")
      scores += svmRanker.scoresOf(qid).toArray
      })

    print("\n")

    // Enrique's code
    for((q,r) <- questions zip scores) yield {
      val pred = r.indexOf(r.max)
      // Plug svm_rank numerical score into the choices of the question object
      val newChoices = q.choices.zip(r).map{
        case (choice, score) => Answer(choice.text, score)
      }

      Question(q.id, q.question, newChoices, Some(pred))
    }
  }

  def N(p: Path) = {
    val heights = Array.ofDim[Double](p.size)
    val linkWeights = Map("hypernym" -> 1, "holonym" -> 1, "hyponym" -> -1, 
      "meronym" -> -1, "similar" -> 0)
    val _p = p.reverse
    heights(0) = 1
    for(i <- 1 until _p.size)
      heights(i) = linkWeights(_p(i-1).get._2.get._1) + heights(i-1) // NextHop

    val rescaled = rescale(heights, Double.MinPositiveValue, 1.0)
    val sorted = rescaled.toSet.toArray.sorted
    val min = if(sorted.isEmpty) Double.MinPositiveValue else sorted(0)
    val nextMin = if(sorted.size > 1) sorted(1) else min
    rescaled.zipWithIndex.foreach{
      case (h, i) => if(h == min) rescaled(i) = nextMin/2.0
    }

    rescaled.map(1.0/_)
  }

  def S(q: Question, a: Answer, p: Path): (Array[Node],Array[Double]) = {
    val q2v = avgVector(sentence2set(q.annotation.get.sentences)._1
      .toArray.map(w2v(_)))
    val a2v = avgVector(sentence2set(a.annotation.get.sentences)._1
      .toArray.map(w2v(_)))
    val qaSum = plus(q2v,a2v)
    Utils._w2v.norm(qaSum)

    val _N = N(p)

    val scores = p.elems.reverse.toArray.map(_._1).zipWithIndex.map{ 
      case (node, idx) => {
      val nodeSum = plus(node.def2v, node.set2v)
      Utils._w2v.norm(nodeSum)      
      (node,dotProduct(qaSum, nodeSum)*_N(idx))
      }
    }
    val _scores = rescale(rescale(scores.map(_._2), 
      Double.MinPositiveValue, 1.0).map(Math.log(_)),
      Double.MinPositiveValue, 1.0)
    
    (scores.map(_._1), _scores)
  }

  def mkFeatures(G: Option[Graph[Node,WkLkDiEdge]], q: Question, a: Answer) = {
    val (qWords, qTags) = sentence2set(q.annotation.get.sentences)
    val (aWords, aTags) = sentence2set(a.annotation.get.sentences)
    
    val _qWordSet = words2set(qWords, qTags, G.get)
    val _aWordSet = words2set(aWords, aTags, G.get)
    val common = _qWordSet.intersect(_aWordSet)
    val qWordSet = _qWordSet -- common
    val aWordSet = _aWordSet -- common

    val _paths = ArrayBuffer[Queue[Path]]()

    qWordSet.foreach(q => {
      aWordSet.foreach(a => _paths += GraphUtils.genAllPaths(G.get, q, a))
    })

    val allPaths = _paths.foldLeft(Array[Path]())(
      (queue, paths) => queue ++ paths)

    val ns = allPaths.map(S(q, a, _))
    val (nodes, scores) = (ns.map(_._1), ns.map(_._2))
    val summedScores = scores.map(_.sum)
    Utils._w2v.norm(summedScores)

    val numPaths = allPaths.size
    
    val avgPathL = (numPaths != 0) match {
      case true => allPaths.map(_.elems.size).sum/(1.0*allPaths.size)
      case false => 0.0
    }

    val avgScore = summedScores.size != 0 match {
      case true => summedScores.foldLeft(0.0)(_ + _)/(1.0*summedScores.size)
      case false => 0.0
    }

    val maxScore = if(summedScores.isEmpty) 0.0 else summedScores.max 
    val maxScoreLength = if(summedScores.isEmpty) 0.0 
      else allPaths(summedScores.indexOf(maxScore)).elems.size
    
    val minScore = if(summedScores.isEmpty) 0.0 else summedScores.min
    val minScoreLength = if(summedScores.isEmpty) 0.0
      else allPaths(summedScores.indexOf(minScore)).elems.size
    
    val minPathL = if(allPaths.isEmpty) 0.0 else allPaths.map(_.elems.size).min
    val minPathLScore = if(summedScores.isEmpty) 0.0
      else {
        if(allPaths.isEmpty) 0.0 else {
          summedScores(allPaths.indexOf(
            allPaths.find(_.elems.size == minPathL).get))
        }
      }

    val maxPathL = if(allPaths.isEmpty) 0.0 else allPaths.map(_.elems.size).max
    val maxPathLScore = if(summedScores.isEmpty) 0.0
      else {
        if(allPaths.isEmpty) 0.0 else {
          summedScores(allPaths.indexOf(
            allPaths.find(_.elems.size == maxPathL).get))
        }
      }
    
    val gNodeSize = G.get.nodes.size
    val gEdgeSize = G.get.edges.size

    val aggregatedLinks = allPaths.foldLeft(Array[String]())(
      (links, paths) => links ++ paths.elems.map(_._2 match {
        case Some(o) => o._1
        case None => ""
        }))

    val hyperCount = aggregatedLinks.filter(_.equals("hypernym")).size
    val hypoCount = aggregatedLinks.filter(_.equals("hyponym")).size
    val holoCount = aggregatedLinks.filter(_.equals("holonym")).size
    val meroCount = aggregatedLinks.filter(_.equals("meronym")).size

    val hyper_hypo = if(hypoCount == 0) 0.0 else 1.0*hyperCount/hypoCount
    val holo_mero = if(meroCount == 0) 0.0 else 1.0*holoCount/meroCount

    val counter = new Counter[String]
    counter.incrementCount("num_paths", numPaths)
    counter.incrementCount("avg_score", avgScore)
    counter.incrementCount("avg_path_len", avgPathL)
    counter.incrementCount("min_score", minScore)
    counter.incrementCount("min_score_length", minScoreLength)
    counter.incrementCount("max_score", maxScore)
    counter.incrementCount("max_score_length", maxScoreLength)
    counter.incrementCount("min_path_len", minPathL)
    counter.incrementCount("min_path_len_score", minPathLScore)
    counter.incrementCount("max_path_len", maxPathL)
    counter.incrementCount("max_path_len_score", maxPathLScore)
    counter.incrementCount("hyper_hypo", hyper_hypo)
    counter.incrementCount("holo_mero", holo_mero)
    counter.incrementCount("node_size", gNodeSize)
    counter.incrementCount("edge_size", gEdgeSize)
    counter
  }

  def createFeatureVector(question:String,
    choice:String, queryRes:QueryResult):Seq[Double] = Seq()

  def train(questions:Seq[Question],
    index:IRIndex, outputFile:Option[File] = None, 
    normalizersFile:Option[File] = None) = {

    BottomUpClassify.annotateQuestions(questions)

    var done = 0
    val total = questions.size
    val id = (Thread.currentThread.getId)%numThreads.get
    val name = (Thread.currentThread.getName)

    val tw = new BufferedWriter(new FileWriter(s"${config.getString("graph.keepAlive")}_$id.out"))
    questions.foreach(q => {
      val G = Some(GraphUtils.load(s"${config.getString("graph.folder")}/${q.id}.json"))
      val qid = q.choices.zipWithIndex.reverse.foldLeft(
        List[Datum[Int, String]]())(
          (answers, choice) => 
            new RVFDatum[Int, String](if(choice._2 == q.rightChoice.get) 1 else 0,
              mkFeatures(G, q, choice._1)) :: answers)
      this.synchronized {
        dataset += qid
      }
      done += 1
      try {
        tw.write(q.id + "\n")
        tw.flush()
        } catch {
          case io: IOException =>
        }

      print(s"$done/$total Qs added to graph\r")
      })
    

    print("\n")
    tw.write(name + " waiting at barrier\n"); tw.close()

    // Wait until other threads finish
    println(name + " waiting at barrier")
    this.barrier.get.await()
    
    // Let only one thread do this
    if(id == 0) {
      scaleRange = Some(Datasets.svmScaleRankingDataset[Int, String](
        dataset, 0.0, 1.0))
      svmRanker.train(dataset)
    }

    // Wait until thread 0 finishes training
    this.barrier.get.await()
  }

  def load(file:File, normalizersFile:Option[File] = None) = {}

  def downloadGraph(questions: Seq[Question]) = {
    // Init graph
    var done = 0
    val total = questions.size

    BottomUpClassify.annotateQuestions(questions)

    questions.foreach(q => {
      val (qWords, qTags) = sentence2set(q.annotation.get.sentences)
      val _G = GraphUtils.mkGraph(qWords, qTags)
      q.choices.foreach(ans => {
        val (aWords, aTags) = sentence2set(ans.annotation.get.sentences)
        _G ++= GraphUtils.mkGraph(aWords, aTags)
        })
      GraphUtils.saveTo(_G, s"${config.getString("graph.folder")}/${q.id}.json")
      done += 1
      print(s"$done/$total Qs added to graph\r")
    })
    print("\n")    
  }
} 