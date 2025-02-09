name := "fpinscala"

ThisBuild / scalaVersion := "3.6.3"

ThisBuild / scalacOptions += "-Wunused:all"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-Xkind-projector:underscores",
  "-source:3.6-migration",
  "-rewrite"
)

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
