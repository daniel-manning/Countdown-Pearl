name := "Countdown"

version := "0.1.1"

scalaVersion := "3.2.2"

val scalacticVersion = "3.2.15"
libraryDependencies += "org.scalactic" %% "scalactic" % scalacticVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalacticVersion % Test
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % scalacticVersion % Test