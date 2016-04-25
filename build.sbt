name := "cs665"

version := "1.0"

scalaVersion := "2.11.8"

javaOptions ++= Seq("-Xmx20G")

libraryDependencies ++= Seq(
  //"org.clulab" %% "processors" % "5.8.0",
  //"org.clulab" %% "processors" % "5.8.0" classifier "models",
  "org.clulab" %% "processors" % "5.8.2",
  "org.clulab" %% "processors" % "5.8.2" classifier "models",
  "com.typesafe" % "config" % "1.2.1",
  "commons-io" % "commons-io" % "2.4",
  "org.apache.lucene" % "lucene-core" % "5.3.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "5.3.1",
  "org.apache.lucene" % "lucene-queryparser" % "5.3.1",
  "com.assembla.scala-incubator" %% "graph-core" % "1.11.0",
  "com.assembla.scala-incubator" %% "graph-json" % "1.11.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.11.0",
  "net.liftweb" %% "lift-json" % "2.0",
  "com.thoughtworks.paranamer" % "paranamer" % "2.8",
  "net.sf.extjwnl" % "extjwnl" % "1.9.1",
  "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2"
)
