organization := "org.dots42"

name := "pariprasna"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.4.1",
  "io.circe" %% "circe-parser" % "0.4.1",
  "org.http4s" %% "http4s-servlet" % "0.13.2a",
  "org.http4s" %% "http4s-blaze-client" % "0.13.2a",
  "org.http4s" %% "http4s-circe" % "0.13.2a",
  "org.specs2" %% "specs2-core" % "3.7.2" % "test",
  "org.typelevel" %% "scalaz-specs2" % "0.4.0" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

lazy val iaksmlka = ProjectRef(uri("https://github.com/dozed/iaksmlka.git"), "iaksmlka")

lazy val pariprasna = project.in(file("."))
  .dependsOn(iaksmlka)
