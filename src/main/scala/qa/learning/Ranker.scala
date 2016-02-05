package qa.learning

trait Ranker{
    // Call svm_rank or something
    def rerank(list:Seq[DataPoint]):Seq[DataPoint]
}

object RankerFactory{
    def get(name:String):Ranker = {
        name match {
            case "svm" => new DummyRanker
            case "perceptron" => new DummyRanker
            case "dummy" => new DummyRanker
            case _ => new DummyRanker
        }
    }
}

class DummyRanker extends Ranker{
    def rerank(list:Seq[DataPoint]):Seq[DataPoint] = list
}
