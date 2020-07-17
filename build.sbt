name := "PPS_VikingChess"

version := "0.1"

scalaVersion := "2.12.5"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.12.5",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.12" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test,
      "org.scalamock" %% "scalamock" % "4.4.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "it.unibo.alice.tuprolog" % "tuprolog" % "3.1" ,
      "com.typesafe.akka" %% "akka-actor" % "2.6.6",
      "org.scala-lang.modules" %% "scala-async" % "0.10.0"

    ),
    crossPaths := false,
    Test / parallelExecution := false
  )