scalaVersion := "2.12.8"

name := "mpc4s"
organization := "com.ruimo"
version := "1.0-SNAPSHOT"

//libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
// lazy val root = (project in file(".")).
//   settings(
//     inThisBuild(List(
//       organization := "ch.epfl.scala",
//       scalaVersion := "2.12.8"
//     )),
//     name := "hello-world"
//   )

publishTo := Some(
  Resolver.file(
    "mpc4s",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.26"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

// https://mvnrepository.com/artifact/org.specs2/specs2-core
libraryDependencies += "org.specs2" %% "specs2-core" % "4.5.1" % Test
libraryDependencies += "com.ruimo" %% "scoins" % "1.22"
