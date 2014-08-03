name := "scala-exercises"

version := "0.0.1"

scalaVersion := "2.11.2"

val scalacOpts = Seq("")

val scaladocOpts = Seq("-diagrams")

val deps = Seq("org.scalatest" % "scalatest_2.11" % "2.2.0" % "test")

libraryDependencies ++= deps

scalacOptions ++= scalacOpts

scalacOptions in (Compile, doc) ++= scaladocOpts
