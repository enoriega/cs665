package qa.ir

import qa.input.Question

class DummyIndex(name:String) extends IRIndex(name) {
    def query(question:String, choice:String):Seq[Document] = Seq(Document("This is some text", 0.87))
}
