name := """ThingsNet"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).dependsOn(
  ProjectRef(uri("https://github.com/w3c/banana-rdf.git"), "banana-rdf"),
  ProjectRef(uri("https://github.com/w3c/banana-rdf.git"), "banana-jena")
)

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.apache.jena" % "jena-tdb" % "1.0.1"
)
