package qa.qtype

import java.io.File
import java.util.Properties

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.learning.RVFDataset._
import edu.arizona.sista.learning._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import qa.input.{Question, InputReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Created by bsharp on 3/3/16.
  */


object BottomUpClassify extends App {

  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  lazy val processor = new FastNLPProcessor()
  val featureExtractor = new FeatureExtractor(config)
  val RESCALE_LOWER = config.getDouble("buc.rescale_lower")
  val RESCALE_UPPER = config.getDouble("buc.rescale_upper")

  val useLength:Boolean = true
  val useNGram:Boolean = false
  val useKeyWord:Boolean = true



  // Opens a file, reads the questions and divides them into train, dev, and test partitions
  def generateFoldsFromFile(fn:String):(Seq[Question], Seq[Question], Seq[Question]) = {
    val questions = new InputReader(new File(fn)).qTypeQuestions
    val numQuestions = questions.length
    // Grab the first 60% questions for train
    val (train, other) = questions.partition(questions.indexOf(_) <= (0.6*numQuestions))
    val numOther = other.length
    // Divide the rest into dev and test
    val (dev, test) = other.partition(other.indexOf(_) <= (0.5*numOther))

    println (s"After partitioning the $numQuestions questions...")
    println (s"Train Questions: ${train.length}")
    println (s"Dev Questions: ${dev.length}")
    println (s"Test Questions: ${test.length}")

    (train, dev, test)
  }

  // Make a Dataset for training out of the training questions
  def mkRVFDataset(questions:Seq[Question]):RVFDataset[Int, String] = {
    val dataset = new RVFDataset[Int, String]()

    // Annotate the questions
    annotateQuestions(questions)

    // For each question, make a datum and add it to the dataset
    for (q <- questions) dataset += mkDatumFromQuestion(q)

    //val informativeness = Datasets.sortFeaturesByInformativeness(dataset, 2)

    // Gather what's needed to filter ngram features by PMI
    val nonLemmaFeatures = getNonLemmaFeatureNames(dataset)
    val labelProbs = getLabelProbs(dataset)
    val lemmasToKeep = filterLemmasByPMI(questions, dataset.labelLexicon.size, keepPercentage = 0.05, labelProbs)
    val featuresToKeep = lemmasToKeep ++ nonLemmaFeatures
    val filteredDataset = filterDatasetByPMI(dataset, featuresToKeep)

    println(s"A total of ${filteredDataset.featureLexicon.size} features were extracted (and in use in the dataset).")
    filteredDataset
  }

  def annotateQuestions(questions:Seq[Question]): Seq[Question] = {
    for (q <- questions){
      q.annotation = Some(mkPartialAnnotation(q.question))
      for (a <- q.choices) a.annotation = Some(mkPartialAnnotation(a.text))
    }
    questions
  }

  def mkPartialAnnotation(text:String): Document = {
    val doc = processor.mkDocument(text)
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    processor.recognizeNamedEntities(doc)
    processor.parse(doc)
    doc.clear()
    doc
  }

  // Given a Question, extract the features and return a Datum
  def mkDatumFromQuestion(q:Question,
                          scaleRange: Option[ScaleRange[String]] = None,
                          lower:Double = 0.0,
                          upper:Double = 1.0):RVFDatum[Int, String] = {
    // Get label
    val label = q.rightChoice.getOrElse(-1)
    assert (label != -1)
    // Extract Features
    val questionFeatures = mkFeatures(q)

    val datum = new RVFDatum[Int, String](label, questionFeatures)

    // Rescale if doing so
    if (scaleRange.isDefined) {
      val scaledFeatures = Datasets.svmScaleDatum(datum.featuresCounter, scaleRange.get, lower, upper)
      return new RVFDatum[Int, String](label, scaledFeatures)
    }

    datum
  }

  // Extract a set of features from the question
  def mkFeatures(q:Question):Counter[String] = {
    val qDoc = q.annotation.get
    val aDocs = q.choices.map(a => a.annotation.get)
    val fc = new Counter[String]

    // Length Features:
    if (useLength) {
      fc.setCount("numSentsInQuestion", featureExtractor.numSentsInQuestion(qDoc))
      fc.setCount("avgWordsInEachAnswer", featureExtractor.avgWordsInAns(aDocs))
    }

    // Ngram Features:
    if (useNGram) {
      featureExtractor.addUnigramsFeatures(Seq(qDoc) ++ aDocs, fc, filterPOS = true)
    }

    // Key Word Features:
     if (useKeyWord) {
       featureExtractor.addKeyWordFeatures(Seq(qDoc) ++ aDocs, fc)
     }

    fc
  }

  def getNonLemmaFeatureNames(dataset:RVFDataset[Int, String]): collection.Set[String] = {
    for {
      fName <- dataset.featureLexicon.keySet
      if !fName.startsWith("LEMMA")
    } yield fName
  }

  def getLabelProbs(dataset:RVFDataset[Int, String]):Array[Double] = {
    val labelProbs = Array.fill[Double](dataset.labelLexicon.size)(0.0)
    for (label <- dataset.labels) labelProbs(label) += 1.0 / dataset.size.toDouble
    println ("Label Distribution calculated: ")
    for (label <- labelProbs.indices) println (s"Label $label probability: ${labelProbs(label)}")
    labelProbs
  }

  def filterLemmasByPMI(questions:Seq[Question], numLabels:Int, keepPercentage:Double, labelProb:Array[Double]):Set[String] = {
//                    P(x|y)
//    pmi(x ,y) = log ------- / (-log(P(x, y)))
//                     p(x)

    // Get the probability and conditional probabilities of each lemma
    val (lemmaProb, lemmaProbGivenLabel) = getLemmaProbability(questions, numLabels)

    // Calulate the PMIs for each lemma
    val pmisBylabel = new Array[Counter[String]](numLabels)
    for (label <- 0 until numLabels) {
      pmisBylabel(label) = new Counter[String]
      val condProbLemma = lemmaProbGivenLabel(label)
      for (x <- condProbLemma.keySet) {
        val normalizer:Double = - math.log(condProbLemma.getCount(x) * labelProb(label))
        val pmi:Double = math.log(condProbLemma.getCount(x)/lemmaProb.getCount(x)) / normalizer
        pmisBylabel(label).setCount(x, pmi)
      }
    }

    // Sort the lemmas by PMIs for each label and determine which to keep
    val pmisToKeep = new ArrayBuffer[(Double, String)]
    for (label <- 0 until numLabels) {
      // Initialize storage array
      val unsorted = new ArrayBuffer[(Double, String)]
      val pmis = pmisBylabel(label)

      // Add the lemmas and pmis
      pmis.keySet.foreach(lemma => unsorted.append((pmis.getCount(lemma), lemma)))

      // Determine which lemmas you're keeping
      val sorted = unsorted.sortBy(- _._1)
      val numLemmas = sorted.length
      val keepThreshold = numLemmas * keepPercentage
      val keepSlice = sorted.slice(0, math.floor(keepThreshold).toInt)

      pmisToKeep.insertAll(pmisToKeep.length, keepSlice)
    }

    println ("Filtering by PMI finished.")
    pmisToKeep.map(pmi => pmi._2).toSet
  }

  def filterDatasetByPMI(dataset:RVFDataset[Int, String], featuresToKeep:Set[String]): RVFDataset[Int, String] = {

    val counts = dataset.countFeatures(dataset.features)
    println("Total unique features before filtering: " + counts.size)

    val featuresToKeepByIndex = featuresToKeep.map(f => dataset.featureLexicon.get(f).getOrElse(-1))
    for (f <- featuresToKeepByIndex) if (f == -1) println(f)

    // map old feature ids to new ids, over the filtered set
    val featureIndexMap = new mutable.HashMap[Int, Int]()
    var newId = 0
    for(f <- 0 until dataset.featureLexicon.size) {
      if(featuresToKeep.contains(dataset.featureLexicon.get(f))) {
        featureIndexMap += f -> newId
        newId += 1
      }
    }

    // construct the new dataset with the filtered features
    val newFeatures = new ArrayBuffer[Array[Int]]
    val newValues = new ArrayBuffer[Array[Double]]()
    for(i <- 0 until dataset.size) {
      val feats = dataset.features(i)
      val vals = dataset.values(i)
      val (filteredFeats, filteredVals) = removeByName(feats, vals, featuresToKeepByIndex, featureIndexMap)
      newFeatures += filteredFeats
      newValues += filteredVals
    }
    logger.debug("Total features after filtering: " + dataset.countFeatures(newFeatures).size)

    new RVFDataset[Int, String](dataset.labelLexicon, dataset.featureLexicon.mapIndicesTo(featureIndexMap.toMap), dataset.labels, newFeatures, newValues)
  }

  private def removeByName(fs:Array[Int],
                           vs:Array[Double],
                           lemmas:Set[Int],
                           featureIndexMap:mutable.HashMap[Int, Int]):(Array[Int], Array[Double]) = {
    val filteredFeats = new ArrayBuffer[Int]()
    val filteredVals = new ArrayBuffer[Double]()
    for(i <- fs.indices) {
      val f = fs(i)
      val v = vs(i)
      if(lemmas.contains(f)) {
        assert(featureIndexMap.contains(f))
        filteredFeats += featureIndexMap.get(f).get
        filteredVals += v
      }
    }
    (filteredFeats.toArray, filteredVals.toArray)
  }

  // Given a set of labeled training questions, returns the
  def getLemmaProbability(questions:Seq[Question], numLabels:Int): (Counter[String], Array[Counter[String]]) = {
    val probsOverall = new Counter[String]
    val probsByLabel = new Array[Counter[String]](numLabels)
    for (i <- 0 until numLabels) probsByLabel(i) = new Counter[String]

    for (q <- questions) {
      val label = q.rightChoice.get
      val qDoc = q.annotation.get
      val aDocs = q.choices.map(a => a.annotation.get)
      for {
        doc <- Array(qDoc) ++ aDocs
        s <- doc.sentences
        l <- s.lemmas.get
      } {
        probsOverall.incrementCount("LEMMA_" + l)
        probsByLabel(label).incrementCount("LEMMA_" + l)
      }
    }

    // Mark lowest frequency as "UNKNOWN"
//    replaceWithUnknown(probsOverall, 2.0, "UNKNOWN")
//    probsByLabel.foreach(replaceWithUnknown(_, 2.0, "UNKNOWN"))

    // Normalize to get probability
    val totalCountsOverall = getTotalCount(probsOverall)
    for (k <- probsOverall.keySet) {
      probsOverall.setCount(k, probsOverall.getCount(k) / totalCountsOverall)
    }
    // Normalize to get probability
    val totalsByLabel = probsByLabel.map(getTotalCount(_))
    for (label <- 0 until numLabels) {
      for (k <- probsByLabel(label).keySet) {
        probsByLabel(label).setCount(k, probsByLabel(label).getCount(k) / totalsByLabel(label))
      }
    }

    (probsOverall, probsByLabel)
  }

  def replaceWithUnknown[A](counter:Counter[A], threshold:Double = 1.0, unknown:A) = {
    for {
      k <- counter.keySet
      if counter.getCount(k) < threshold
    } {
      counter.setCount(k, 0.0)
      counter.incrementCount(unknown)
    }
  }


  // Returns the sum of the values stored in a Counter
  def getTotalCount[A](c:Counter[A]): Double = {
    var total:Double = 0.0
    for {
      k <- c.keySet
      v = c.getCount(k)
    } total += v

    total
  }


  // Determine how many of the predictions were correct
  def evaluate[L,F](datums:Seq[RVFDatum[L,F]], predictedLabels:Seq[L]): Double = {
    var precisionAt1:Double = 0.0
    val numDatums:Double = datums.length.toDouble

    // Increment the P@1 if we get the label correct
    // TODO: not handling ties, but currently not an issue (only two labels)
    for {
      i <- datums.indices
      gold = datums(i).label
      if predictedLabels(i) == gold
    } precisionAt1 += (1.0 / numDatums)

    precisionAt1
  }

  // Main method


  // 1. Load the training dataset
  val (trainQuestions, devQuestions, testQuestions) = generateFoldsFromFile(config.getString("buc.questions"))
  val trainDataset = mkRVFDataset(trainQuestions)

  // 2. Scale the features
  println ("Before scaling: " + trainDataset.featuresCounter(0).toString)
  val rescale = config.getBoolean("buc.rescale")
  val scaleRange = if (rescale) Some(Datasets.svmScaleRVFDataset(trainDataset, RESCALE_LOWER, RESCALE_UPPER)) else None
  println ("After scaling: " + trainDataset.featuresCounter(0).toString)
//  sys.exit()

  // 3. Train
  val classifier = new LogisticRegressionClassifier[Int, String](bias = true)
//  val classifier = new LinearSVMClassifier[Int, String]()
//  val props = new Properties
//  props.setProperty("epochs", "20")
//  props.setProperty("burnInIterations", "100")
//  props.setProperty("marginRatio", "1.0")
//  val classifier = new PerceptronClassifier[Int, String](props)
  classifier.train(trainDataset)

  // 4. Predict
  val useDev = config.getBoolean("buc.useDev")
  if (useDev) println ("Testing on DEV") else println("Testing on **TEST**")
  val annotatedQuestions = if (useDev) annotateQuestions(devQuestions) else annotateQuestions(testQuestions)
  val testDatums = annotatedQuestions.map(q => mkDatumFromQuestion(q, scaleRange, RESCALE_LOWER, RESCALE_UPPER))
  val predictedLabels = testDatums.map(td => classifier.classOf(td))

  // 5. Evaluate
  val precisionAt1 = evaluate[Int,String](testDatums, predictedLabels)
  println ("Final Precision: " + precisionAt1)


}
