name := "TestIt"

version := "0.1"

scalaVersion := "2.11.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  "org.scala-lang" % "scala-library" % scalaVersion.value % "provided",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
