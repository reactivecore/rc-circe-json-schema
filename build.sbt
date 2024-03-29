lazy val scala212 = "2.12.17"
lazy val scala213 = "2.13.10"
lazy val scala3 = "3.2.2"

lazy val supportedScalaVersions = List(scala212, scala213, scala3)

ThisBuild / scalaVersion := scala3

ThisBuild / organization := "net.reactivecore"

ThisBuild / fork := false
ThisBuild / Test / fork := true

val circeVersion = "0.14.5"

git.baseVersion := "0.4"

enablePlugins(GitVersioning)

ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/reactivecore/rc-circe-json-schema"))
ThisBuild / developers := List(
  Developer(
    id = "nob13",
    name = "Norbert Schultz",
    email = "norbert.schultz@reactivecore.de",
    url = url("https://www.reactivecore.de")
  )
)
ThisBuild / testOptions  += Tests.Argument("-oDF")

usePgpKeyHex("77D0E9E04837F8CBBCD56429897A43978251C225")

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test,
  "org.scalatest" %% "scalatest-flatspec" % "3.2.15" % Test,
  "commons-io" % "commons-io" % "2.11.0" % Test
)

lazy val schema = (project in file("schema"))
  .settings(name := "circe-json-schema")
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    ),
    libraryDependencies ++= testDependencies,
    crossScalaVersions := supportedScalaVersions,
    Compile / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n <= 12 => List("-Ypartial-unification")
        case _                       => Nil
      }
    }
  )

lazy val examples = (project in file("examples"))
  .dependsOn(schema)
  .settings(
    name := "examples",
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    crossScalaVersions := supportedScalaVersions
  )

lazy val root = (project in file("."))
  .aggregate(
    schema,
    examples
  )
  .settings(
    name := "circe-json-schema-root",
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    test := {},
    crossScalaVersions := Nil
  )
