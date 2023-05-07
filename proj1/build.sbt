scalaVersion := "2.12.0"

scalacOptions in ThisBuild ++= Seq("-language:reflectiveCalls", "-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

logBuffered in Test := false

parallelExecution in Test := false
