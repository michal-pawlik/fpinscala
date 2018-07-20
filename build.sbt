val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val practiceCodeSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  ),
  resolvers ++= Seq(
    "Artima Maven Repository" at "http://repo.artima.com/releases"
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
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

lazy val practice = (project in file("PracticeCode"))
  .settings(commonSettings, practiceCodeSettings)
  .settings(
    name := "practice"
  )
