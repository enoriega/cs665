package qa.util

import qa.paths._
import qa.input._
import qa.learning._
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.{Callable, CyclicBarrier, Future}

class ThreadController(pr: PathRanker, questions: Seq[Question],
  isTrain: Boolean, poolSize: Int) extends Callable[Seq[Question]] {
  val pool = Executors.newFixedThreadPool(poolSize)
  def call(): Seq[Question] = {
    val futureSet = ArrayBuffer[Future[Option[Seq[Question]]]]()
    try { 
      for(i <- 0 until poolSize) {
        futureSet += pool.submit(new ThreadRunner(pr, questions, 
          isTrain, poolSize))
      }
    } catch {
      case ie: InterruptedException => println("Something got interrupted")
    } finally {
      pool.shutdown()
    }


    futureSet.foldLeft(Array[Question]())(
      (questions, future) => future.get match {
        case Some(q) => questions ++ q
        case None => questions
        })
  }
}

class ThreadRunner(pr: PathRanker, questions: Seq[Question], 
  isTrain: Boolean, N: Int) extends Callable[Option[Seq[Question]]] {
  def call(): Option[Seq[Question]] = {
    val id = Thread.currentThread.getId.toInt % N
    val window = questions.size/N
    val lower = id*window
    val upper = if(id == N-1) questions.size else (id+1)*window
    println(s"$id: [$lower, $upper)")
    val subQuestions = questions.slice(lower, upper)
    //(new PathRanker).downloadGraph(subQuestions)

    if(isTrain) {
      pr.train(subQuestions, null)
      None
    }
    else Some(pr.rerank(subQuestions, null))
  }
}
