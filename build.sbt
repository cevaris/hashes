name := "hashes"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.twitter" %% "util-logging" % "6.29.0"

)

resolvers in ThisBuild ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)