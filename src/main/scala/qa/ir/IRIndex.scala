package qa.ir

import java.io.File
import qa.input.Question

// Models a returned document from an IR Index
case class Document(val text:String, val score:Double) {}

// API to query an index
abstract class IRIndex(val name:String){
    def query(question:String, choice:String):Seq[Document]
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
