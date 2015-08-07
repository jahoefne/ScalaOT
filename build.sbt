enablePlugins(ScalaJSPlugin)

name := "Scalot Operational Transformation for ScalaJS"

normalizedName := "scalot"

version := "1.0"

organization := "com.jahoefne"

scalaVersion := "2.11.7"

licenses += ("BSD-style", url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("http://github.com/jahoefne"))

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
    <scm>
      <url>git@github.com:jahoefne/Scalot.git</url>
      <connection>scm:git:git@github.com:jahoefne/Scalot.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jahoefne</id>
        <name>Jan Hoefner</name>
        <url>http://github.com/jahoefne</url>
      </developer>
    </developers>)