import sbt._

lazy val V = new {
  val scala = "2.13.6"
  val ZIO = "2.0.0-M3"
}

lazy val functionalDesign = (project in file(".")).
  settings (
    name := "Functional Design",
    organization := "net.degoes",
    version := "0.1-SNAPSHOT",
    scalaVersion := V.scala
    // add other settings here
  )

/* scala versions and options */
scalaVersion := V.scala

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation"
  , "-unchecked"
  , "-encoding", "UTF-8"
  , "-Xlint"
  , "-Xverify"
  , "-feature"
  //,"-Ypartial-unification" - not needed for scala-2.13.x
  //,"-Xfatal-warnings" // Recommend enable before you commit code
  , "-language:_"
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


libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  // -- testing --
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  // -- Logging --
  "ch.qos.logback" % "logback-classic" % "1.2.6",

  // This pulls in cats and fs2
  "org.creativescala" %% "doodle" % "0.9.25",

  // scalaz
  "dev.zio" %% "zio" % V.ZIO,
  "dev.zio" %% "zio-streams" % V.ZIO,

)

