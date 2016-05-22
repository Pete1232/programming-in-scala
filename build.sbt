lazy val settings = Seq(
  organization := "pete1232",
  version := "1.0.0",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "2.2.6" % "test",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "ch.qos.logback" %  "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
  ),
  scalacOptions ++= Seq("-feature", "-language:postfixOps")
)

lazy val ProgrammingInScala = (project in file(".")).
  settings(settings: _*)
