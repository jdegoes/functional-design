import sbt._

lazy val functionalDesign = (project in file(".")).settings(
  name := "Functional Design",
  organization := "net.degoes",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.13.3"
  // add other settings here
)

/* scala versions and options */
scalaVersion := "2.13.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding",
  "UTF-8",
  "-Xlint",
  "-Xverify",
  "-feature"
  //,"-Xfatal-warnings" // Recommend enable before you commit code
  ,
  "-language:_"
  //,"-optimise"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

// javaOptions in Universal ++= Seq(
//   "-J-server",
//   "-J-Xms1g -Xmx4g",
//   "-J-XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled",
//   "-J-XX:+UseCMSInitiatingOccupancyOnly -XX:CMSInitiatingOccupancyFraction=68",
//   "-J-XX:+ScavengeBeforeFullGC -XX:+CMSScavengeBeforeRemark",
//   "-J-XX:+UseGCLogFileRotation -XX:NumberOfGCLogFiles=10 -XX:GCLogFileSize=100M"
// )

val CatsVersion       = "2.1.0"
val CatsEffectVersion = "2.1.1"
val MonixVersion      = "3.1.0"
val ZIOVersion        = "1.0.0"
val ShapelessVersion  = "2.3.3"
val FS2Version        = "2.2.2"
val AmmoniteVersion   = "2.2.0"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  // -- testing --
  "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
  "org.scalatest"  %% "scalatest"  % "3.2.0"  % "test",
  // -- Logging --
  "ch.qos.logback"             % "logback-classic" % "1.1.3",
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
  // Cats
  "org.typelevel" %% "cats-core"   % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  // fs2
  "co.fs2" %% "fs2-core" % FS2Version,
  // monix
  "io.monix" %% "monix" % MonixVersion,
  // shapeless
  "com.chuusai" %% "shapeless" % ShapelessVersion,
  // scalaz
  "dev.zio" %% "zio"         % ZIOVersion,
  "dev.zio" %% "zio-streams" % ZIOVersion,
  // type classes
  "org.typelevel" %% "simulacrum" % "1.0.0",
  // li haoyi ammonite repl embed
  "com.lihaoyi" % "ammonite" % AmmoniteVersion % "test" cross CrossVersion.full
)

resolvers ++= Seq(
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Secured Central Repository" at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)

// ammonite repl
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue
