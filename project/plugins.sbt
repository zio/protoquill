resolvers += Classpaths.sbtPluginReleases

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addDependencyTreePlugin

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.12")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
