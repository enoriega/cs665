package qa.paths

import qa.util._
import qa.input._
import qa.learning._
import java.io.File
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import java.util.concurrent.{Executors, ExecutorService}

class ThreadController(questions: Seq[Question], 
  poolSize: Int) extends Runnable {
  val pool = Executors.newFixedThreadPool(poolSize)
  def run() = {
    try { 
      for(i <- 0 until poolSize)
        pool.execute(new ThreadRunner(questions, poolSize))

    } catch {
      case ie: InterruptedException => println("Something got interrupted")
    } finally {
      pool.shutdown()
    }
  }
}

class ThreadRunner(questions: Seq[Question], N: Int) extends Runnable {
  def run() = {

    val id = Thread.currentThread.getId.toInt % N
    val window = questions.size/N
    val subQuestions = questions.slice(id*window, (id+1)*window)
    (new PathRanker).downloadGraph(subQuestions)
  }
}

object TestFeatures extends App {
  val config = ConfigFactory.load
  val pr = new PathRanker
  val ir1 = new InputReader(new File(config.getString("trainingFile")))
  val ir2 = new InputReader(new File(config.getString("devFile")))
  val ir3 = new InputReader(new File(config.getString("testFile")))
  val questions = ir1.questions ++ ir2.questions ++ ir3.questions
  
  /* Download 10k questions */
  (new ThreadController(questions, 32)).run
}