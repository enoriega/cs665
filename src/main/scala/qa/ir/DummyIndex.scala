package qa.ir

import qa.input.Question

class DummyIndex(name:String) extends IRIndex(name, null) {
    def query(question:String, choice:String) = QueryResult(1, 1, Seq(Document("This is some text", 0.87)))
    def numDocs = 0
}
