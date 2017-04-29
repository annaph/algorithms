name := "dwarfs-rafting"
organization := "com.algorithms.dwarfs.rafting"
version := "1.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:postfixOps")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "junit" % "junit" % "4.10" % "test")
