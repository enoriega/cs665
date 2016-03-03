package qa.learning

import java.io.File

import qa.input.Question
import qa.ir.{QueryResult, IRIndex}

/**
  * Created by bsharp on 2/9/16.
  */
class W2VRanker extends Ranker {
  def rerank(list:Seq[Question], index:IRIndex):Seq[Question] = {
    ???
  }


  // Extract features from retrived docs
  def createFeatureVector(question:String,
                          choice:String, queryRes:QueryResult):Seq[Double] = {


    ???
  }

  // Train the model, save the result to file if provided
  def train(questions:Seq[Question], index:IRIndex,
            outputFile:Option[File] = None, normalizersFile:Option[File] = None):Unit = {
    ???
  }

  // Load an already trained model to this instance
  def load(file:File, normalizersFile:Option[File] = None):Unit = ???
}
