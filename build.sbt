import sbt._

val ZIOVersion = "2.0.9"
val ScalaVersionConstant = "3.3.0-RC6"

lazy val functionalDesign = (project in file(".")).
  settings (
    name := "Functional Design",
    organization := "net.degoes",
    version := "0.2",
    scalaVersion := ScalaVersionConstant,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )

/* scala versions and options */
scalaVersion := ScalaVersionConstant

scalacOptions ++= Seq("-rewrite", "-indent")

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation"
  , "-unchecked"
  , "-encoding", "UTF-8"
  , "-feature"
  , "-language:_"
  , "-Wconf:msg=a type was inferred to be `Any`:silent"
  , "-source:future"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")



libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % ZIOVersion,
  "dev.zio" %% "zio-streams"  % ZIOVersion,
  "dev.zio" %% "zio-test"     % ZIOVersion  % Test,
  "dev.zio" %% "zio-test-sbt" % ZIOVersion  % Test
)

resolvers ++= Seq(
  "Secured Central Repository" at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)