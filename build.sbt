name := "SUDOKU Solver"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "org.jline" % "jline" % "3.12.1"

libraryDependencies += "org.jline" % "jline-terminal" % "3.12.1"

libraryDependencies += "org.jline" % "jline-reader" % "3.12.1"
libraryDependencies += "org.jline" % "jline-terminal-jansi" % "3.12.1"

val buildSettings =  Seq(
  javaOptions += "-Djline.terminal=jline.UnixTerminal",
)


