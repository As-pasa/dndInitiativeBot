ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "TgBot2"
  )
// Core with minimal dependencies, enough to spawn your first bot.
libraryDependencies += "com.bot4s" %% "telegram-core" % "5.7.1"

// Extra goodies: Webhooks, support for games, bindings for actors.
libraryDependencies += "com.bot4s" %% "telegram-akka" % "5.7.1"

// https://mvnrepository.com/artifact/biz.enef/slogging
libraryDependencies += "biz.enef" %% "slogging" % "0.6.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

