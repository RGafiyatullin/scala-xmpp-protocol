name := "xmpp-protocol"

organization := "com.github.rgafiyatullin"

version := "0.4.0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
scalacOptions ++= Seq("-language:implicitConversions")
scalacOptions ++= Seq("-Ywarn-value-discard", "-Xfatal-warnings")

publishTo := {
  Some("releases"  at "https://artifactory.wgdp.io:443/xmppcs-maven-releases/")
}
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials.wg-domain")

libraryDependencies ++= Seq(
  "org.scalatest"                 %% "scalatest"  % "3.0.4",
  "com.github.rgafiyatullin"      %% "xml"        % "0.2.0.2"
)
