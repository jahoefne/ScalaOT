name := "Scalot"

version := "1.0"

organization := "com.jahoefne"

scalaVersion := "2.11.7"

val http4sVersion = "0.6.1"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.http4s" %% "http4s-blazeserver" % http4sVersion,
  "org.http4s" %% "http4s-core" % http4sVersion,
  "org.http4s" %% "http4s-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-argonaut" % http4sVersion,
  "com.lihaoyi" %% "scalatags" % "0.5.2"
)