import sbt.Keys._
import sbt._


lazy val commonSettings: Seq[Setting[_]] = Seq(
  name := "pointfree-lift",
  version := "1.0",
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq("-feature", "-unchecked")
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(pointfree, scurses, onions)

lazy val pointfree = (project in file("pointfree"))
  .settings(commonSettings: _*)
  .dependsOn(onions)
  .settings(
    name := "pointfree",
    libraryDependencies += "junit" % "junit" % "4.11",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.18",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    libraryDependencies += "org.jline" % "jline" % "3.5.1",
    mainClass in(Compile, run) := Some("pointfree.Interactive")
  )

lazy val scurses = (project in file("scurses"))
  .settings(commonSettings: _*)
  .settings(
    name := "Scurses",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0",
    mainClass in(Compile, run) := Some("net.team2xh.scurses.examples.GameOfLife")
  )

lazy val onions = (project in file("onions"))
  .settings(commonSettings: _*)
  .dependsOn(scurses)
  .settings(
    name := "Onions",
    mainClass in(Compile, run) := Some("net.team2xh.onions.examples.ExampleUI")
  )