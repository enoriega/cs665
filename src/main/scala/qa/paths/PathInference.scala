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
  val reader = new InputReader(new File(config.getString("trainingFile")))
  val questions = reader.questions
  def main(args: Array[String]) = {
    val pr = new PathRanker
    pr.train(questions, null)
  }
}