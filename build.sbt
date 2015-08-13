version := "0.2"

lazy val scalot = crossProject
  /* .crossType(CrossType.Pure)*/
  .settings(
    name := "scalot",
    organization := "com.github.jahoefne",
    version := "0.3.4",
    scalaVersion := "2.11.7",

    unmanagedSourceDirectories in Compile += {
      val v = if (scalaVersion.value startsWith "2.10.") "scala-2.10" else "scala-2.11"
      baseDirectory.value / ".." / "shared" / "src" / "main" / v
    },

    publishArtifact in Test := false,
    publishTo := {
      if (isSnapshot.value)
        Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
      else
        Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    autoCompilerPlugins := true,

    publishMavenStyle := true,
    pomExtra :=
      <url>http://github.com/jahoefne</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>http://www.opensource.org/licenses/bsd-license.php</url>
          </license>
        </licenses>
        <scm>
          <url>git://github.com:jahoefne/Scalot.git</url>
          <connection>scm:git://github.com:jahoefne/Scalot.git</connection>
        </scm>
        <developers>
          <developer>
            <id>jahoefne</id>
            <name>Jan Hoefner</name>
            <url>http://github.com/jahoefne</url>
          </developer>
        </developers>
  )
  .jsSettings()
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.3.4",
      "com.lihaoyi" %% "utest" % "0.3.1")
  )

lazy val scalotJVM = scalot.jvm
lazy val scalotJS = scalot.js
