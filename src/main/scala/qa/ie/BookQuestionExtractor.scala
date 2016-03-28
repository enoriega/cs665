package qa.ie

import java.io.File
import io.Source
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{Document, Sentence}

/** Enumerates the documents over a "Becky's" book text file
 *  @Author Enrique Noriega <enoriega@email.arizona.edu>
 */
object BookQuestionExtractor extends App{
    val filePath = args(0)

    val proc = new FastNLPProcessor(withDiscourse=false)

    val k = 5

    // Filter for questions and incomplete sentences
    val marks = Set("?", ":", ";")

    // Parse the lines and annotate the docs
    val lines = Source.fromFile(filePath).getLines.toList.filter{
      line =>
        val last = line.takeRight(1)
        !marks.contains(last)
    }



    val paragraphs:Map[String, Document] = lines.map{
      l =>
        val tokens = l.split("\t")
        (tokens(0), tokens(1))
    }.groupBy{
      case (id, txt) =>
        val key = id.split('.').dropRight(1).mkString(".")
        key
    }.mapValues{
      tuples =>
        val sentences = tuples.map(_._2)
        val doc = proc.annotateFromSentences(sentences)
        doc
    }


    val d = paragraphs.values.toList

    val questions:Seq[ArtificialQA] = d.flatMap{
      doc =>
      try{
        doc.sentences.zipWithIndex map {
          case (sen, ix) =>
            makeQuestion(sen, ix, doc)
        }
      } catch{
        case _:Throwable => Seq()
      }
    }.filter{
      case n:NoQA => false
      case _ => true
    }

    for(question <- questions){
      println(s"${question.question}\t${question.answer}")
    }

    def makeQuestion(sen:Sentence, index:Int, context:Document):ArtificialQA = {

      sen.stanfordBasicDependencies match {
        case Some(deps) =>
          val root:Int = deps.roots.head // I assume there's a single root
          val tags:Seq[String] = sen.tags match { case Some(t) => t; case None => Seq()}
          // If the root is a verb, go ahead
          if (tags(root).startsWith("VB")){

            val allEdges = deps.outgoingEdges
            val edges = allEdges(root)

            // Find the subject
            val subj = edges filter (e => e._2 == "nsubj")
            if(subj.length == 1){
                // Build the question
                val verb = sen.lemmas.get(root)
                val words = sen.words
                val qType = s"NNP"

                val answerNums = expand(subj(0)._1, allEdges)

                val answerTags = answerNums.map(tags).toSet

                val filtered = Set("PRP", "DT")
                if(answerTags.size == 1 && filtered.contains(answerTags.head)){
                  new NoQA
                }
                else{
                  // Filter out all those questions that don't have a noun and a verb
                  val answerNumsSet = answerNums.toSet
                  val questionNums = (0 to words.size - 1).filter(!answerNumsSet.contains(_))

                  val f = questionNums.map(tags).toSet.map{
                    t:String => t.startsWith("V") || t.startsWith("N")
                  }.fold(false)(_||_)

                  if(f){

                    val answer = answerNums.sorted.map(words).mkString(" ")

                    val question = (0 to words.size - 1).map{
                      ix =>
                        if(!answerNumsSet.contains(ix))
                          words(ix)
                        else
                          List.fill(words(ix).size)("_").mkString

                    }.mkString(" ")

                    // TODO: Add justification
                    val justification = ""//justify(sen, index, context)

                    if(!question.contains(" Figure "))
                      ArtificialQA(qType, question, answer, justification)
                    else
                      new NoQA

                  }
                else{
                  new NoQA
                }

                }

            }
            else
              new NoQA
          }
          else
            new NoQA
        case None => new NoQA
      }
    }

    def expand(ix:Int, edges:Array[Array[(Int, String)]]):Seq[Int] = {
      def helper(ix:Int, edges:Array[Array[(Int, String)]]):Seq[Int] = {
        // Select the nodes that traverse the tree using inorder walk
        val e = edges(ix).sortWith(_._1 < _._1)

        if(e.length  == 0)
          Seq(ix)
        else{
          val pre = e.filter(_._1 < ix) flatMap {
            x:(Int, String) =>
              helper(x._1, edges)
          }

          val post = e.filter(_._1 > ix) flatMap {
            x:(Int, String) =>
              helper(x._1, edges)
          }
          pre ++ Seq(ix) ++ post
        }
      }

      val nums = helper(ix, edges)
      nums
    }
}
