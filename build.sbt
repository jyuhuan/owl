name := "owl"

version := "0.0.2-SNAPSHOT"

organization := "me.yuhuan"

scalaVersion := "2.11.7"

publishMavenStyle := true

isSnapshot := true

scalacOptions in (Compile, doc) += "-diagrams"

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

//region Custom Dependencies
//endregion
//endregion


pomExtra :=
  <url>https://github.com/jyuhuan/owl</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:jyuhuan/owl.git</url>
      <connection>scm:git:git@github.com:jyuhuan/owl.git</connection>
    </scm>
    <developers>
      <developer>
        <id>yuhuan</id>
        <name>Yuhuan Jiang</name>
        <url>http://yuhuan.me/</url>
      </developer>
    </developers>
