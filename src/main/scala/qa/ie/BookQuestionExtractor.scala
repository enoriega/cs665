package qa.ie

import java.io.File
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.{Document, Sentence}

/** Enumerates the documents over a "Becky's" book text file
 *  @Author Enrique Noriega <enoriega@email.arizona.edu>
 */
object BookQuestionExtractor extends App{
    val filePath = args(0)

    val proc = new FastNLPProcessor(withDiscourse=true)

    val k = 5

    // TODO: Parse the file into "Documents" identified by the numbers in the first column
    val d = Seq()

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
