package qa.ir

import java.io.File
import com.typesafe.config.{ConfigFactory,Config}
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.queryparser.classic.QueryParser;

class WikipediaIndex(name:String, config:Config) extends IRIndex(name, config) {

  val indexDir = new File(config.getString(s"indexes.$name.indexDir"))

  // Open index
  val topHits = config.getInt(s"indexes.$name.topHits")
  val index = FSDirectory.open(indexDir.toPath)
  val indexReader = DirectoryReader.open(index)
  val indexSearcher = new IndexSearcher(indexReader)

  def query(question:String, choice:String):QueryResult ={
    // Parse the query
    val q = new QueryParser("content", WikipediaIndexer.analyzer).parse(question + "\n" + choice)

    val topDocs = indexSearcher.search(q, topHits);

    QueryResult(topDocs.totalHits, this.numDocs, topDocs.getMaxScore,
      topDocs.scoreDocs map {
        hit =>
          Document(indexSearcher.doc(hit.doc).get("content"), hit.score)
      }
    )
  }

  def numDocs:Int = indexReader.numDocs()
}

// Index the wikipedia with Lucene
object WikipediaIndexer extends App{
  // Read config
  val config = if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val textDir = new File(config.getString("wiki.textDir"))
  val indexDir = new File(config.getString("wiki.indexDir"))

  // Analyzer for reading and writting
  def analyzer = new StandardAnalyzer

  // Create the index writer
  val indexWriter = {
    val index = FSDirectory.open(indexDir.toPath)
    val config = new IndexWriterConfig(analyzer)

    new IndexWriter(index, config)
  }

  //Iterate over all the wiki files
  for(
       dir <- textDir.listFiles if dir.isDirectory;
       file <- dir.listFiles if file.getName.startsWith("wiki_")
     )
  {
    println(s"Processing ${file.getName}...")
    val text = scala.io.Source.fromFile(file).mkString
    val articles = text.split("</?doc.*>").filter(_ != "")

    for(article <- articles){
      val doc = mkDocument(article)
      // Add to the index
      indexWriter.addDocument(doc)
    }
  }

  indexWriter.close

  def mkDocument(text:String):document.Document = {
    val doc = new document.Document();
    doc.add(new TextField("content", text, Field.Store.YES));

    doc
  }

}
