package qa.input

import java.io.{FilenameFilter, File}

/**
  * Created by bsharp on 3/11/16.
  */
object InputUtils {

  // Get all files in a directory ending with a given extension
  def findFiles(collectionDir: String, fileExtension:String): Array[File] = {
    val dir = new File(collectionDir)
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(fileExtension)
    })
  }

}
