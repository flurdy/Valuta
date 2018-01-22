name := """valuta"""
organization := "com.flurdy"

version := "2.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.3"

// Not for release
resolvers += "local-artifactory" at "http://localhost:8181/artifactory/maven-mirror/"

resolvers += "flurdy-maven" at "http://dl.bintray.com/content/flurdy/maven"

libraryDependencies ++= {
   val enumeratumVersion = "1.5.12"
   Seq(
      guice,
      ws,
      "com.flurdy" % "sander-core_2.11" % "0.2.0",
      "com.beachape" %% "enumeratum" % enumeratumVersion,
      "com.beachape" %% "enumeratum-play" % enumeratumVersion,
      "com.github.etaty" %% "rediscala" % "1.8.0",
      "org.webjars" % "bootstrap" % "3.3.7-1",
      "com.github.kstyrc" % "embedded-redis" % "0.6" % Test,
      "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
      "com.flurdy" %% "scalasoup" % "0.1.0" % Test,
      "org.mockito" % "mockito-core" % "2.8.9" % Test,
      "ai.grakn" % "redis-mock" % "0.1.3" % Test
   )
}
