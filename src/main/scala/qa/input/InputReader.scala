package qa.input

import scala.collection.JavaConverters._
import java.io.File
import org.apache.commons.io.FileUtils

// Reads the questions from a file
// THIS MUST BE A VALIDATION OR TEST FILE
// NOT THE TRAINING FILE (it has one more column)

class InputReader(file:File){
    // Read the lines from the file
    val lines = FileUtils.readLines(file).asScala.drop(1) // get rid of the header

    def questions:Seq[Question] = lines map {
        line =>
            val t = line.split('\t')
            t.length match {
              case 6 =>
                Question(t(0), t(1), Seq(t(2), t(3), t(4), t(5)))
              case 7 =>
                val choice = t(2) match {
                  case "A" => 0
                  case "B" => 1
                  case "C" => 2
                  case "D" => 3
                }
                Question(t(0), t(1), Seq(t(3), t(4), t(5), t(6)), Some(choice))
              case num =>
                throw new RuntimeException(s"s{file.getName} has $num columns")
            }

    }
}
