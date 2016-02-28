package qa.ir

import java.io.File
import com.typesafe.config.Config
import qa.input.Question

// Models a returned document from an IR Index
case class Document(val text:String, val score:Double)

// Result of the query
case class QueryResult(numResults:Int, docsInIndex:Int, maxScore:Double, topDocs:Seq[Document])

// API to query an index
abstract class IRIndex(val name:String, config:Config){
    def query(question:String, choice:String):QueryResult
    def numDocs:Int
}

// Returns concrete Implementations of IRIndex
class IndexFactory(dir:File){
    def get(name:String, config:Config):IRIndex = {
        name match {
            case "simple_wiki" => new WikipediaIndex(name, config)
            case "en_wiki" => new WikipediaIndex(name, config)
            case _ => throw new RuntimeException(s"$name is not a valid index type")
        }
    }
}
