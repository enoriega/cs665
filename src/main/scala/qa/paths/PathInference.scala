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
import scala.collection.parallel._

object PathInference {
  val config = ConfigFactory.load
  val trainReader = new InputReader(new File(config.getString("trainingFile")))
  val devReader = new InputReader(new File(config.getString("devFile")))
  val testReader = new InputReader(new File(config.getString("testFile")))
  val artificialReader1 = new InputReader(new File(config.getString("artificialQ1")))
  val artificialReader2 = new InputReader(new File(config.getString("artificialQ2")))
  val trainQs = trainReader.questions
  val devQs = devReader.questions
  val testQs = testReader.questions
  val artificialQA1 = artificialReader1.questions
  val artificialQA2 = artificialReader2.questions

  def eval(src: Seq[Int], pred: Seq[Int]) = {
    val correct = src.zip(pred).foldLeft(0)(
      (count, sp) => if(sp._1 == sp._2) count + 1 else count
    )
    val accuracy = 1.0*correct/src.size
    accuracy
  }

  def unitTest(_c: Double, args: Seq[String], train: Seq[Question],
    test: Seq[Question], isDev: Boolean, isArtificial: Int): Seq[Question] = {
    val pr = new PathRanker(keepFiles = true, c=_c)
    val numThreads = args(0).toInt

    pr.numThreads = Some(numThreads)
    pr.barrier = Some(new CyclicBarrier(numThreads))
    pr.isArtificial = isArtificial
    
    //val questionBuffer = ArrayBuffer[Question]()
    val trainFolds = Range(0,train.size+1,numThreads).toArray
    val testFolds = Range(0,test.size+1,numThreads).toArray

    println("Adding all to dataset...")
    pr.addToDataset(train)

    println("Calling SVMrank train...")
    pr.train

    println("Reranking...")
    val questionBuffer = pr.rerank(test, null)

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
      questionBuffer.foreach(r => bw.write(s"${r.id}, ${choices(r.rightChoice.get)}\n"))
      for((question, qi) <- questionBuffer.zipWithIndex;
          (choice, ci) <- question.choices.zipWithIndex){
            // Format: Question Index \t Answer Index \t Numeric score
            ew.write(s"$qi\t$ci\t${choice.score}\n")
          }

      bw.close()
      ew.close()
      } catch {
        case io: IOException =>
      }


    questionBuffer.toSeq
  }

  def main(args: Array[String]) = {
    //val cFolds = Array(0.1, 1.0, 10.0, 100.0, 1000.0)
    //val acc = Array(0.0, 0.0, 0.0, 0.0, 0.0)

    val trainSet = Array("default" -> trainQs, "qa1" -> artificialQA1, "qa2" -> artificialQA2)
    val acc = Array(0.0)
    //val bestC = cFolds(acc.indexOf(acc.max))
    val _c = 1.0

    val (k,v) = (args(1), trainSet.find(_._1 == args(1)).get._2)

    val rq = unitTest(_c, args, v, devQs, isDev=true, isArtificial=trainSet.indexOf((k,v)))
    val src = devQs.map(_.rightChoice.get)
    val pred = rq.map(_.rightChoice.get)
    acc(0) = eval(src, pred)
    println(s"Accuracy for c=${_c}, trainSet=${k} is ${acc(0)}")
    val aw = new BufferedWriter(new FileWriter(
      s"${config.getString("BASE_DIR")}_${k}.out"))
    aw.write(acc.mkString(", ")+"\n")
    

    if(args(1).equals("default")) {
      val _8kOut = unitTest(_c, args, trainQs++devQs, testQs, isDev=false, 0)
    }
    aw.close()
  }
}