ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-encoding", "UTF-8"
  //"-unchecked",
  //"-Xlint",
  //"-Ywarn-dead-code"
)

lazy val root = (project in file("."))
  .enablePlugins(AssemblyPlugin)
  .settings(
    name := "ScalaLearn",
    assembly / mainClass := Some("basic.App"),
    assembly / assemblyJarName := "app.jar",
    assembly / assemblyMergeStrategy := {
      case "module-info.class" => MergeStrategy.discard // Discard the module-info.class file
      case x if x.endsWith("module-info.class") => MergeStrategy.discard // Optionally discard all "module-info.class" files
      case _ => MergeStrategy.first // Default merge strategy for other files
    }
  )

//shellPrompt := { s => Project.extract(s).currentProject.id + "> " }

lazy val logbackVersion = "1.5.12"
lazy val slf4jVersion = "2.0.16"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "org.slf4j" % "jul-to-slf4j" % slf4jVersion,
  "org.slf4j" % "slf4j-api" % slf4jVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "com.google.guava" % "guava" % "33.2.1-jre",
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
)