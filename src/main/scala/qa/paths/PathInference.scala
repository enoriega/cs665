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
import java.io.File

object PathInference {
  val config = ConfigFactory.load
  val trainReader = new InputReader(new File(config.getString("trainingFile")))
  val devReader = new InputReader(new File(config.getString("devFile")))
  val trainQs = trainReader.questions
  val devQs = devReader.questions
  def main(args: Array[String]) = {
    val pr = new PathRanker
    pr.train(trainQs, null)
    val pr2 = new PathRanker
    pr2.train(devQs, null)
  }
}