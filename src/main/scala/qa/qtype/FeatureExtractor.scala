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
      w = s.words(i).toLowerCase
      l = s.lemmas.get(i).toLowerCase()
      if !FeatureExtractor.STOP_WORD_LEMMAS.contains(l)
      t = s.tags.get(i)
      if !filterPOS || isContentTag(t)
    } counter.incrementCount("LEMMA_" + l, 1.0)
  }

  // Look for lemmas that are specifiec in a set of key words, add if found
  def addKeyWordFeatures(docs:Seq[Document], counter:Counter[String]): Unit = {
    for {
      d <- docs
      s <- d.sentences
      i <- s.words.indices
      w = s.words(i).toLowerCase
      l = s.lemmas.get(i).toLowerCase()
      if !FeatureExtractor.STOP_WORD_LEMMAS.contains(l)
      t = s.tags.get(i)
      if FeatureExtractor.KEY_LEMMAS.contains(l)
    } counter.incrementCount("KEYWORD_" + l, 1.0)
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

  //def KEY_LEMMAS = Array(KEY_LEMMAS_INFTYPE, KEY_LEMMAS_QUANTIFIER, KEY_LEMMAS_OTHER).flatten
//  def KEY_LEMMAS = Array(KEY_LEMMAS_INFTYPE, KEY_LEMMAS_OTHER).flatten
  //def KEY_LEMMAS = Array(KEY_LEMMAS_INFTYPE).flatten
  //def KEY_LEMMAS = Array(KEY_LEMMAS_QUANTIFIER).flatten
  def KEY_LEMMAS = Array(KEY_LEMMAS_OTHER).flatten


  def KEY_LEMMAS_INFTYPE = Array("example", "cause", "because", "result", "lead", "process")

  def KEY_LEMMAS_QUANTIFIER = Array("not", "none", "all", "some", "never", "increase", "decrease",
    "more", "less", "rate")

  def KEY_LEMMAS_OTHER = Array("cycle", "__________", "example")


}
