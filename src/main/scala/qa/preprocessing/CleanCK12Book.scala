// package qa.preprocessing
//
// import java.io.{FilenameFilter, File, PrintWriter}
//
// import edu.arizona.sista.embeddings.word2vec.Word2Vec
// import edu.arizona.sista.struct.Counter
// import qa.input.InputUtils
//
// /**
//   * Created by bsharp on 3/7/16.
//   */
// // object CleanCK12Book extends App {
// //
// //   def readLines(fn:String): Seq[(String, String, String)] = {
// //     val lines = scala.io.Source.fromFile(fn, "UTF-8").getLines()
// //     val split = lines.map(l => l.split("\t"))
// //     val out = split.map(a => (sanitizeHeader(a(0)), a(0), a(1)))
// //     out.toSeq
// //   }
// //
// //   // Returns a counter of the number of times the *sanitized* header appears in the data
// //   def countHeaders (data:Seq[(String, String, String)]):Counter[String] = {
// //     val counter = new Counter[String]
// //     for (item <- data) {
// //       val sanitized = item._1
// //       counter.incrementCount(sanitized)
// //     }
// //     counter
// //   }
// //
// //   def sanitizeHeader(h:String):String = {
// //     val san1 = h.split(" ").map(w => Word2Vec.sanitizeWord(w))
// //     val san2 = san1.map(w => condenseRomans(w))
// //     san2.mkString(" ")
// //   }
// //
// //   def condenseRomans(s:String): String = {
// //     for (i <- 1 to 4) {
// //       if (s == "i"*i) return "i"
// //     }
// //     s
// //   }
// //
// //   // Returns the keys which have a value higher than n:Double
// //   def filterByCount(counter:Counter[String], n:Double, exclude:Seq[String], include:Seq[String]): Seq[String] = {
// //     val pass1 = counter.keySet.filter(k => counter.getCount(k) > n)
// //     val pass2 = pass1.diff(exclude.toSet)
// //     val pass3 = pass2.toSeq ++ include
// //     pass3
// //   }
// //
// //   def removeBullet(s:String):String = {
// //     val split = s.split(" ")
// //     val kept = split.filter(word => !word.contains("•"))
// //     kept.mkString(" ")
// //   }
// //
// //   def filterByContains(s:String, exclude:Seq[String]):Boolean = {
// //     for (w <- exclude) {
// //       if (s.contains(w)) return false
// //     }
// //     true
// //   }
// //
// //   val dir = "/home/bsharp/ck12_8thScience-27Feb-Wenli/clean_file/"
// //
// //   val filesIn = InputUtils.findFiles(dir, ".clean").map(f => f.getAbsolutePath)
// //
// //   val fnIn = "/home/bsharp/ck12_8thScience-27Feb-Wenli/clean_file/CK-12-Life-Science-Concepts-For-Middle-School.txt.clean"
// //
// //
// //   val headers = new Counter[String]
// //   for (file <- filesIn) {
// //     val data = readLines(fnIn)
// //     val currHeaders = countHeaders(data)
// //     val currTopHeaders = headers.topKeys(20)
// //     println("")
// //     println ("*****************")
// //     println("")
// //     currTopHeaders.foreach(h => println(h + ": " + currHeaders.getCount(h)))
// //     println("")
// //     headers += currHeaders
// //
// //   }
// //
// //     println("")
// //   println ("*****************")
// //   println("")
// //   val topHeaders = headers.topKeys(50)
// //   topHeaders.foreach(h => println(h + ": " + headers.getCount(h)))
// //   println("")
// //
// //
// //   val stopWords = Array("by-nc", "public domain", "cc by xnumx",
// //     "bync xnumx", "used under license from", "by xnumx", "bync")
// //
// //   // Find the headers which occur very frequently, these indicate that the section is not contentful
// //   val filteredOut = filterByCount(headers, 1000.0,
// //                                   exclude = Array("summary"),
// //                                   include = stopWords ++ Array("domain", "examples", "characteristics", "ff", "bb"))
// //
// //   filteredOut.foreach(fo => println("Filtering Out: " + fo + " (" + headers.getCount(fo) + ")"))
// //
// //   for (file <- filesIn) {
// //     val fnOut = file + ".cleaner"
// //     val pw = new PrintWriter(fnOut)
// //     val data = readLines(fnIn)
// //     // Check to see if header is in filtered out
// //     for (d <- data) {
// //       // If the current header is not filtered out:
// //       if (!filteredOut.contains(d._1)) {
// //         if (filterByContains(d._1, stopWords) && filterByContains(d._3, stopWords)) {
// //           if (d._3.trim().split(" ").length > 1) {
// //             pw.println(d._2.trim + "\t" + removeBullet(d._3).trim)
// //           }
// //         }
// //       }
// //     }
// //     pw.close()
// //   }
// //
// //
// //
// //
// // }
