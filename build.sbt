name := "SUDOKU Solver"

version := "1.0"

scalaVersion := "2.12.6"
//libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

// https://mvnrepository.com/artifact/org.jline/jline
libraryDependencies += "org.jline" % "jline" % "3.13.0"
libraryDependencies += "org.jline" % "jline-terminal" % "3.13.0"

//libraryDependencies += "jline" % "jline-reader" % "2.14.6"
//libraryDependencies += "jline" % "jline-terminal-jansi" % "2.14.6"

// https://mvnrepository.com/artifact/org.processing/core
libraryDependencies += "org.processing" % "core" % "2.2.1"


val buildSettings =  Seq(
  javaOptions += "-Djline.terminal=jline.UnixTerminal",
)


