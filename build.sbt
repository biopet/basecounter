organization := "com.github.biopet"
//TODO: change name
name := "tool-template"

scalaVersion := "2.11.11"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.github.broadinstitute" % "picard" %  "2.11.0"
libraryDependencies += "com.github.biopet" %% "tool-utils" % "0.1-SNAPSHOT" changing()
libraryDependencies += "com.github.biopet" %% "ngs-utils" % "0.1-SNAPSHOT" changing()

libraryDependencies += "com.github.biopet" %% "test-utils" % "0.1-SNAPSHOT" % Test changing()

//TODO: change mainClass
mainClass in assembly := Some("nl.biopet.tools.basecounter.BaseCounter")

useGpg := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

import ReleaseTransformations._
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
