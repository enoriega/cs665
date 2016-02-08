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
    val rankerOutputDir = new File(config.getString("rankerOutputDir"))
    if (!rankerOutputDir.exists) {
        println(s"creating ${rankerOutputDir.getCanonicalPath}")
        FileUtils.forceMkdir(rankerOutputDir)
    } else if (!rankerOutputDir.isDirectory) {
        sys.error(s"${rankerOutputDir.getCanonicalPath} is not a directory")
    }

    // Read input into "objects"
    val questions = new InputReader(new File(config.getString("inputFile"))).questions

    val choiceMap = Map(0 -> 'A', 1 -> 'B', 2 -> 'C', 3 -> 'D')
    // Query IR for multiple data bases
    val indexNames = config.getStringList("indexNames").asScala


    // Load the lucene indexes
    val ixFactory = new IndexFactory(new File(config.getString("luceneDir")))
    val indexes = for(indexName <- indexNames) yield {
        ixFactory.get(indexName, config)
    }

    // Create the output files, one per index
    val outputFiles = (for(indexName <- indexNames) yield {
        (indexName -> new FileWriter(new File(rankerOutputDir, indexName)))
    }).toMap

    // Instantiate the ranker
    val ranker = new RankerFactory(config).get(config.getString("ranker"))

    // Do the mojo
    for{
        index <- indexes
    }  {
        // Rank them
        val rankedQuestions = ranker.rerank(questions, index)

        // Write the results to TSV files on disk
        val file = outputFiles(index.name)
        // TODO: Output the files for the voter
        for(question <- rankedQuestions){
          val choice = question.rightChoice match {
            case Some(c) => c
            case None => -1
          }
          file.write(s"${question.id}\t$choice\n")
        }
    }

    // Don't forget to close the files!!
    outputFiles.values foreach (_.close)
}
