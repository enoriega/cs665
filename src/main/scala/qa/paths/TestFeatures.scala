package qa.paths

import qa.util._
import qa.input._
import qa.learning._
import java.io.File
import com.typesafe.config.ConfigFactory

object TestFeatures extends App {
  val config = ConfigFactory.load
  val pr = new PathRanker
  val ir1 = new InputReader(new File(config.getString("trainingFile")))
  val ir2 = new InputReader(new File(config.getString("devFile")))
  val ir3 = new InputReader(new File(config.getString("testFile")))
  val questions = ir1.questions ++ ir2.questions ++ ir3.questions
  
  /* Download 10k questions */
  val future = (new ThreadController(pr, questions, false, 16)).call
}