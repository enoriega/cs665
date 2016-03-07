package qa.ie

import qa.ir.IndexFactory
import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{Document, Sentence}

/** Enumerates the documents over a lucene index and extracts candidate questions
 *  @Author Enrique Noriega <enoriega@email.arizona.edu>
 */
object QuestionExtractor extends App{
    val config = if (args.isEmpty) ConfigFactory.load()
            else ConfigFactory.parseFile(new File(args(0))).resolve()

    val proc = new FastNLPProcessor(withDiscourse=true)
    val ixFactory = new IndexFactory(new File(config.getString("luceneDir")))
    val indexReader = ixFactory.get("simple_wiki", config)
    val k = 5
    val d = indexReader.allDocs.take(1000)

    val questions:Seq[ArtificialQA] = d flatMap  {
      txt =>
      try{
        val doc = proc.annotate(txt)
        doc.sentences map {
          sen => 
            makeQuestion(sen, Seq())
        }
      } catch{
        case _:Throwable => Seq()
      }
    }

    println(s"${questions.size}")

    def makeQuestion(sen:Sentence, context:Seq[Sentence]):ArtificialQA = {
      return new NoQA
    }

}

case class ArtificialQA(val qtype:String, val question:String, val answer:String, val justification:String)
class NoQA extends ArtificialQA("","","","")

