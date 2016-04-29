package qa.paths

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.WkLkDiEdge
import scalax.collection.edge.Implicits._
import scala.collection.mutable.{Set, Queue, ArrayBuffer}
import qa.input._
import qa.util._
import qa.learning._
import com.typesafe.config.{ConfigFactory, Config}
import java.io._
import java.util.concurrent.CyclicBarrier

object PathInference {
  val config = ConfigFactory.load
  val trainReader = new InputReader(new File(config.getString("trainingFile")))
  val devReader = new InputReader(new File(config.getString("devFile")))
  val testReader = new InputReader(new File(config.getString("testFile")))
  val trainQs = trainReader.questions
  val devQs = devReader.questions
  val testQs = testReader.questions

  def eval(src: Seq[Int], pred: Seq[Int]) = {
    val correct = src.zip(pred).foldLeft(0)(
      (count, sp) => if(sp._1 == sp._2) count + 1 else count
    )
    val accuracy = 1.0*correct/src.size
    accuracy
  }

  def unitTest(_c: Double, args: Seq[String], train: Seq[Question],
    test: Seq[Question], isDev: Boolean): Seq[Question] = {
    val pr = new PathRanker(keepFiles = true, c=_c)
    val numThreads = args(0).toInt

    pr.numThreads = Some(numThreads)
    pr.barrier = Some(new CyclicBarrier(numThreads))
    
    val futureOne = (new ThreadController(pr, train,
      isTrain=true, numThreads)).call
    val rq = (new ThreadController(pr, test, 
      isTrain=false, numThreads)).call

    try {
      var fin = ""
      var fin_e = "" 
      if(isDev) {
        fin = s"${config.getString("graph.opt")}_${_c}.out"
        fin_e = s"${config.getString("graph.ensembleOpt")}_${_c}.out"
      } 
      else {
        fin = s"${config.getString("graph._8kOut")}_${_c}.out"
        fin_e = s"${config.getString("graph._8kEOut")}_${_c}.out"
      }

      val bw = new BufferedWriter(new FileWriter(fin))
      val ew = new BufferedWriter(new FileWriter(fin_e))

      val choices = Array("A", "B", "C", "D")
      rq.foreach(r => bw.write(s"${r.id}, ${choices(r.rightChoice.get)}\n"))
      for((question, qi) <- rq.zipWithIndex;
          (choice, ci) <- question.choices.zipWithIndex){
            // Format: Question Index \t Answer Index \t Numeric score
            ew.write(s"$qi\t$ci\t${choice.score}\n")
          }

      bw.close()
      ew.close()
      } catch {
        case io: IOException =>
      }


    rq
  }

  def main(args: Array[String]) = {
    val cFolds = Array(0.1, 1.0, 10.0, 100.0, 1000.0)
    val acc = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    cFolds.zipWithIndex.foreach{case (_c,i) => {
      val rq = unitTest(_c, args, trainQs, devQs, isDev=true)
      val src = devQs.map(_.rightChoice.get)
      val pred = rq.map(_.rightChoice.get)
      acc(i) = eval(src, pred)
      println(s"Accuracy for c=${_c} is ${acc(i)}")
      }}
    
    val bestC = cFolds.indexOf(acc.max)
    val aw = new BufferedWriter(new FileWriter(
      s"${config.getString("graph.accuracies")}"))
    aw.write(acc.mkString(", ")+"\n")
    aw.close()

    val _8kOut = unitTest(bestC, args, trainQs++devQs, testQs, isDev=false)
  }
}