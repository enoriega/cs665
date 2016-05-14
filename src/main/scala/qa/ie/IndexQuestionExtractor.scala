package qa.ie

import qa.ir.IndexFactory
import java.io._
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.struct.DirectedGraph

/** Enumerates the documents over a lucene index and extracts candidate questions
 *  @Author Enrique Noriega <enoriega@email.arizona.edu>
 */
object QuestionExtractor extends App{
    val config = if (args.isEmpty) ConfigFactory.load()
            else ConfigFactory.parseFile(new File(args(0))).resolve()

    val proc = new FastNLPProcessor(withDiscourse=false)
    val ixFactory = new IndexFactory(new File(config.getString("luceneDir")))
    val indexReader = ixFactory.get("simple_wiki", config)
    val k = 5
    val d = indexReader.allDocs.take(100)

    val candidateQuestions:Seq[ArtificialQA] = d flatMap  {
      txt =>
      try{
        val doc = proc.annotate(txt)
        doc.sentences.zipWithIndex map {
          case (sen, ix) =>
            makeQuestion(sen, ix, doc)
        }
      } catch{
        case _:Throwable => Seq()
      }
    }

    val questions = candidateQuestions.filter{
      q =>
        q match {
          case n:NoQA => false
          case a:ArtificialQA => true
        }
    }

    println(s"Got ${questions.size} questions")
    val pw = new PrintWriter("questions.txt")
    questions foreach {
      q =>
        pw.write(s"${q.qtype} - ${q.question} - ${q.answer}\n")
    }
    pw.close

    def makeQuestion(sen:Sentence, index:Int, context:Document):ArtificialQA = {

      sen.dependencies match {
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
              // Find the prepositional phrase
              val prep = edges filter (e => e._2.startsWith("prep_"))
              if(prep.length == 1){
                // Build the question
                val verb = sen.lemmas.get(root)
                val words = sen.words
                val prepType = prep(0)._2.split("_")(1)
                val qType = s"${verb}_$prepType"

                // TODO: Formulate a question string

                val question = expand(prep(0)._1, allEdges, words)
                val answer = expand(subj(0)._1, allEdges, words)

                // TODO: Add justification
                val justification = ""//justify(sen, index, context)

                ArtificialQA(sen.getSentenceText, qType, question, answer, Seq(), Seq(), justification=justification)
              }
              else
                new NoQA
            }
            else
              new NoQA
          }
          else
            new NoQA
        case None => new NoQA
      }
    }

    def expand(ix:Int, edges:Array[Array[(Int, String)]], words:Seq[String]):String = {
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
      nums.map(words(_)).mkString(" ")
    }

}
