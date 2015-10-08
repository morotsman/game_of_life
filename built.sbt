val scala_swing = "org.scala-lang.modules" % "scala-swing_2.11" % "1.0.1"

lazy val root = (project in file(".")).
  settings(
    name := "game_of_life",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies +=scala_swing
  )
