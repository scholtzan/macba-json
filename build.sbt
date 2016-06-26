name := "macba-json"

version := "0.1.0"

organization := "net.scholtzan"


scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:experimental.macros")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.json4s" %% "json4s-ast" % "3.3.0"
)

// testing libraries
libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest" % "2.2.4" % "test"
)

