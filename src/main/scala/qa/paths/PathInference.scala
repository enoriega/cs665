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

object PathInference {
  val config = ConfigFactory.load
  val trainReader = new InputReader(new File(config.getString("trainingFile")))
  val devReader = new InputReader(new File(config.getString("devFile")))
  val trainQs = trainReader.questions
  val devQs = devReader.questions
  def main(args: Array[String]) = {
    val pr = new PathRanker(keepFiles = true)
    pr.train(trainQs.slice(0,10), null)
    val rq = pr.rerank(devQs.slice(0,10), null)

    val bw = new BufferedWriter(new FileWriter(s"${config.getString("graph.opt")}"))
    val choices = Array("A", "B", "C", "D")
    rq.foreach(r => bw.write(s"${r.id}, ${choices(r.rightChoice.get)}\n"))
    bw.close()
  }
}