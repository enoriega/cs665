name := "cs665"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  //"org.clulab" %% "processors" % "5.8.0",
  //"org.clulab" %% "processors" % "5.8.0" classifier "models",
  "org.clulab" %% "processors" % "5.8.2-SNAPSHOT",
  "org.clulab" %% "processors" % "5.8.2-SNAPSHOT" classifier "models",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.apache.lucene" % "lucene-core" % "5.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "5.3.1"
)
