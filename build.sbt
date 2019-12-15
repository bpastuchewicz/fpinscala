val commonSettings = Seq(
  scalaVersion := "2.12.8",
  resolvers += Resolver.bintrayRepo("johnreed2","maven"),
  libraryDependencies ++= Seq(
    "com.github.johnreedlol" %% "pos" % "2.2.0"
  )
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

