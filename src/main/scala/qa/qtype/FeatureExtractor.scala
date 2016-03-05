package qa.qtype

import com.typesafe.config.Config
import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Counter

/**
  * Created by bsharp on 3/4/16.
  */
class FeatureExtractor (config:Config) {

  /**
    *  Length Features
    */

  def numSentsInQuestion(doc:Document):Double = doc.sentences.length

  // Return the average length (in number of words) of the answer choices
  def avgWordsInAns(docs:Seq[Document]):Double = {
    val wordsInAns = for {
      aDoc <- docs
      s <- aDoc.sentences
    } yield s.words.length

    wordsInAns.sum.toDouble / docs.size.toDouble
  }

  /**
    *  Ngram Features
    */

  def addUnigramsFeatures(docs:Seq[Document], counter:Counter[String], filterPOS:Boolean = false): Unit = {
    // Iterate through the documents and grab the words, add to counter
    for {
      d <- docs
      s <- d.sentences
      i <- s.words.indices
      w = s.words(i)
      l = s.lemmas.get(i)
      if !FeatureExtractor.STOP_WORD_LEMMAS.contains(l)
      t = s.tags.get(i)
      if !filterPOS || isContentTag(t)
    } counter.setCount("LEMMA_" + l, 1.0)
  }

  def isContentTag(t:String):Boolean = {
    for (tagPrefix <- FeatureExtractor.CONTENT_TAGS) {
      if (t.startsWith(tagPrefix)) {
        return true
      }
    }
    false
  }

}

object FeatureExtractor {

  def CONTENT_TAGS = Array("NN", "VB", "RB", "JJ", "IN")

  def STOP_WORD_LEMMAS = Array("be", "have", "do")

}
