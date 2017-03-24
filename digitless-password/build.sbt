name := "digitless-password"
organization := "com.algorithms"
version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
	"-unchecked",
	"-deprecation",
	"-feature",
	"-language:postfixOps")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"junit" % "junit" % "4.10" % "test")
