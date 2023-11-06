name := "gray_scholes"

version := "1.0"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test,
  "io.circe" %% "circe-core" % "0.14.5",
  "io.circe" %% "circe-generic" % "0.14.5",
  "io.circe" %% "circe-parser" % "0.14.5",
  "com.lihaoyi" %% "requests" % "0.8.0",
  "com.lihaoyi" %% "ujson" % "3.1.3",
)
