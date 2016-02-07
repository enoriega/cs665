package qa.ir

import java.io.File
import com.typesafe.config.Config
import qa.input.Question

// Models a returned document from an IR Index
case class Document(val text:String, val score:Double)

// Result of the query
case class QueryResult(numResults:Int, maxScore:Double, topDocs:Seq[Document])

// API to query an index
abstract class IRIndex(val name:String, config:Config){
    def query(question:String, choice:String):QueryResult
}

// Returns concrete Implementations of IRIndex
class IndexFactory(dir:File){
    def get(name:String):IRIndex = {
        name match {
            case "simple-wiki" => new DummyIndex(name)
            case "wiki" => new DummyIndex(name)
            case _ => new DummyIndex(name)
        }
    }
}
