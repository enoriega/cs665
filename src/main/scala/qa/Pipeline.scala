package qa

import scala.collection.JavaConverters._
import qa.input.InputReader
import qa.ir._
import qa.learning._
import java.io.{File, FileWriter}
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }

/** This is the entry point of the system */
object Pipeline extends App {

    // Load the configuration
    // use specified config file or the default one if one is not provided
    val config =
        if (args.isEmpty) ConfigFactory.load()
        else ConfigFactory.parseFile(new File(args(0))).resolve()

    // Create the output dir if it doesn´t exist yet
    val outputDir = new File(config.getString("outputDir"))
    if (!outputDir.exists) {
        println(s"creating ${outputDir.getCanonicalPath}")
        FileUtils.forceMkdir(outputDir)
    } else if (!outputDir.isDirectory) {
        sys.error(s"${outputDir.getCanonicalPath} is not a directory")
    }

    // Read input into "objects"
    val questions = new InputReader(new File(config.getString("inputFile"))).questions

    val choiceMap = Map(1 -> 'A', 2 -> 'B', 3 -> 'C', 4 -> 'D')
    // Query IR for multiple data bases
    val indexNames = config.getStringList("indexNames").asScala

    // Load the lucene indexes
    val ixFactory = new IndexFactory(new File(config.getString("luceneDir")))
    val indexes = for(indexName <- indexNames) yield {
        ixFactory.get(indexName)
    }

    // Create the output files, one per index
    val outputFiles = (for(indexName <- indexNames) yield {
        (indexName -> new FileWriter(new File(outputDir, indexName)))
    }).toMap

    // Instantiate the ranker
    val ranker = RankerFactory.get(config.getString("ranker"))

    // Do the mojo
    for{
        question <- questions
        index <- indexes
    }  {
        // Expand the question/answers into feature vectors
        val featureVectors = FeatureExtractor.makeDataPoint(question, index)
        // Rank them
        val rankedVectors = ranker.rerank(featureVectors)

        // Write the results to TSV files on disk
        val file = outputFiles(index.name)
        file.write(s"${question.id}\t${choiceMap(rankedVectors(0).answerChoince)}\n")
    }

    // Don't forget to close the files!!
    outputFiles.values foreach (_.close)
}
