import com.jsuereth.sbtpgp.PgpKeys.publishSigned

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "io.getquill",
    homepage := Some(url("https://zio.dev/zio-protoquill")),
    licenses := List(("Apache License 2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))),
    developers := List(
      Developer("deusaquilus", "Alexander Ioffe", "", url("https://github.com/deusaquilus"))
    ),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-protoquill"), "git:git@github.com:zio/zio-protoquill.git")
    ),
    versionScheme := Some("always"),
  )
)

addCommandAlias("runCommunityBuild", "; quill-sql/test; quill-sql-tests/test; quill-cassandra/Test/compile")
addCommandAlias("fmt", "all scalafmt test:scalafmt")

val isCommunityBuild =
  sys.props.getOrElse("community", "false").toBoolean

val isCommunityRemoteBuild =
  sys.props.getOrElse("communityRemote", "false").toBoolean

lazy val scalatestVersion =
  if (isCommunityRemoteBuild) "3.2.7" else "3.2.18"

lazy val baseModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-sql`
)

lazy val sqlTestModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-sql-tests`
)

lazy val dbModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-jdbc`, `quill-doobie`, `quill-zio`, `quill-jdbc-zio`, `quill-caliban`
)

lazy val bigdataModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-cassandra`, `quill-cassandra-zio`
)

lazy val allModules =
  baseModules ++ sqlTestModules ++ dbModules ++ bigdataModules

lazy val communityBuildModules =
  Seq[sbt.ClasspathDep[sbt.ProjectReference]](
    `quill-sql`, `quill-sql-tests`, `quill-cassandra`
  )

val filteredModules = {
  val modulesStr = sys.props.get("modules")
  println(s"SBT =:> Modules Argument Value: ${modulesStr}. community=${isCommunityBuild}, communityRemote=${isCommunityRemoteBuild}")

  val selectedModules = modulesStr match {
    case _ if (isCommunityBuild) =>
      println("SBT =:> Doing Community Build! Filtering Community-Build Modules Only")
      communityBuildModules
    case Some("base") =>
      println("SBT =:> Compiling Base Modules")
      baseModules
    case Some("sqltest") =>
      println("SBT =:> Compiling SQL test Modules")
      sqlTestModules
    case Some("db") =>
      println("SBT =:> Compiling Database Modules")
      dbModules
    case Some("bigdata") =>
      println("SBT =:> Compiling Big Data Modules")
      bigdataModules
    case Some("none") =>
      println("SBT =:> Invoking Aggregate Project")
      Seq[sbt.ClasspathDep[sbt.ProjectReference]]()
    case _ =>
      println("SBT =:> No Modules Switch Specified, Compiling All Modules by Default")
      allModules
  }

  println(s"=== Selected Modules ===\n${selectedModules.map(_.project.toString).toList.mkString("\n")}\n=== End Selected Modules ===")
  selectedModules
}

val zioQuillVersion = "4.8.5"
val zioVersion = "2.1.14"

lazy val `quill` =
  (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      publishArtifact := false,
      publish / skip := true,
      publishLocal / skip := true,
      publishSigned / skip := true,
      crossScalaVersions := Nil, // https://www.scala-sbt.org/1.x/docs/Cross-Build.html#Cross+building+a+project+statefully
    )
    .aggregate(filteredModules.map(_.project): _*)

lazy val `quill-sql` =
  (project in file("quill-sql"))
    .settings(commonSettings: _*)
    .settings(
      resolvers ++= Seq(
        Resolver.mavenLocal,
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
        "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
      ),
      excludeDependencies ++= Seq(
        "com.typesafe.scala-logging" % "scala-logging_2.13"
      ),
      libraryDependencies ++= Seq(
        // Needs to be in-sync with both quill-engine and scalafmt-core or ClassNotFound
        // errors will happen. Even if the pprint classes are actually there
        "io.suzaku" %% "boopickle" % "1.5.0",
        "com.lihaoyi" %% "pprint" % "0.9.0",
        "ch.qos.logback" % "logback-classic" % "1.5.16" % Test,
        "io.getquill" %% "quill-engine" % zioQuillVersion,
        "dev.zio" %% "zio" % zioVersion,
        ("io.getquill" %% "quill-util" % zioQuillVersion)
          .excludeAll({
            if (isCommunityBuild)
              Seq(ExclusionRule(organization = "org.scalameta", name = "scalafmt-core_2.13"))
            else
              Seq.empty
          }: _*),
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "org.scalatest" %% "scalatest" % scalatestVersion % Test,
        "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
        "com.vladsch.flexmark" % "flexmark-all" % "0.64.8" % Test
      )
    )

// Moving heavy tests to separate module so it can be compiled in parallel with others
lazy val `quill-sql-tests` =
  (project in file("quill-sql-tests"))
    .settings(publish / skip := true)
    .settings(commonSettings: _*)
    .settings(
      Test / testOptions += Tests.Argument("-oF")
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

//lazy val `quill-sql-all` = (project in file(".")).aggregate(`quill-sql`, `quill-sql-tests`)

lazy val `quill-jdbc` =
  (project in file("quill-jdbc"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .dependsOn(`quill-sql` % "compile->compile;test->test")

ThisBuild / libraryDependencySchemes += "org.typelevel" %% "cats-effect" % "always"
lazy val `quill-doobie` =
  (project in file("quill-doobie"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "org.tpolecat" %% "doobie-core" % "1.0.0-RC5",
        "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC5" % Test
      )
    )
    .dependsOn(`quill-jdbc` % "compile->compile;test->test")

lazy val `quill-caliban` =
  (project in file("quill-caliban"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban-quick" % "2.7.2",
        // Adding this to main dependencies would force users to use logback-classic for SLF4j unless the specifically remove it
        // seems to be safer to just exclude & add a commented about need for a SLF4j implementation in Docs.
        "ch.qos.logback" % "logback-classic" % "1.5.16" % Test,
        // Don't want to make this dependant on zio-test for the testing code so importing this here separately
        "org.scalatest" %% "scalatest" % scalatestVersion % Test,
        "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
        "org.postgresql" % "postgresql" % "42.7.5" % Test,
      )
    )
    .dependsOn(`quill-jdbc-zio` % "compile->compile")

lazy val `quill-zio` =
  (project in file("quill-zio"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-streams" % zioVersion
      )
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-jdbc-zio` =
  (project in file("quill-jdbc-zio"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingLibraries: _*)
    .settings(
      libraryDependencies ++= Seq(
        // Needed for PGObject in JsonExtensions but not necessary if user is not using postgres
        "org.postgresql" % "postgresql" % "42.7.5" % "provided",
        "dev.zio" %% "zio-json" % "0.7.4"
      ),
      Test / runMain / fork := true,
      Test / fork := true,
      Test / testGrouping := {
        (Test / definedTests).value map { test =>
          if (test.name endsWith "IntegrationSpec")
            Tests.Group(name = test.name, tests = Seq(test), runPolicy = Tests.SubProcess(
              ForkOptions().withRunJVMOptions(Vector("-Xmx200m"))
            ))
          else
            Tests.Group(name = test.name, tests = Seq(test), runPolicy = Tests.SubProcess(ForkOptions()))
        }
      }
    )
    .dependsOn(`quill-zio` % "compile->compile;test->test")
    .dependsOn(`quill-sql` % "compile->compile;test->test")
    .dependsOn(`quill-jdbc` % "compile->compile;test->test")

lazy val `quill-cassandra` =
  (project in file("quill-cassandra"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := false,
      libraryDependencies ++= Seq(
        "com.datastax.oss" % "java-driver-core" % "4.17.0"
      )
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-cassandra-zio` =
  (project in file("quill-cassandra-zio"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.datastax.oss" % "java-driver-core" % "4.17.0",
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-streams" % zioVersion
      )
    )
    .dependsOn(`quill-cassandra` % "compile->compile;test->test")
    .dependsOn(`quill-zio` % "compile->compile;test->test")

// Include scalafmt formatter for pretty printing failed queries
val includeFormatter =
  sys.props.getOrElse("formatScala", "false").toBoolean

lazy val commonSettings =
  basicSettings ++ {
    if (isCommunityRemoteBuild)
      Seq(
        Compile / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
      )
    else
      Seq.empty
  }

lazy val jdbcTestingLibraries = Seq(
  // JDBC Libraries for testing of quill-jdbc___ contexts
  libraryDependencies ++= Seq(
    "com.zaxxer" % "HikariCP" % "6.2.1" exclude("org.slf4j", "*"),
    // In 8.0.22 error happens: Conversion from java.time.OffsetDateTime to TIMESTAMP is not supported
    "com.mysql" % "mysql-connector-j" % "9.1.0" % Test,
    "com.h2database" % "h2" % "2.3.232" % Test,
    // In 42.2.18 error happens: PSQLException: conversion to class java.time.OffsetTime from timetz not supported
    "org.postgresql" % "postgresql" % "42.7.5" % Test,
    "org.xerial" % "sqlite-jdbc" % "3.48.0.0" % Test,
    // In 7.1.1-jre8-preview error happens: The conversion to class java.time.OffsetDateTime is unsupported.
    "com.microsoft.sqlserver" % "mssql-jdbc" % "7.4.1.jre11" % Test,
    "com.oracle.ojdbc" % "ojdbc8" % "19.3.0.0" % Test,
    //"org.mockito"             %% "mockito-scala-scalatest" % "1.16.2"              % Test
  )
)

lazy val jdbcTestingSettings = jdbcTestingLibraries ++ Seq(
  Test / fork := true
)

lazy val basicSettings = Seq(
  Test / testOptions += Tests.Argument("-oI"),
  libraryDependencies ++= Seq(
    ("org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2")
  ),
  excludeDependencies ++= Seq(
    ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13")
  ),
  scalaVersion := "3.3.4",
  // The -e option is the 'error' report of ScalaTest. We want it to only make a log
  // of the failed tests once all tests are done, the regular -o log shows everything else.
  // Test / testOptions ++= Seq(
  //   Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  //   //  /*, "-eGNCXEHLOPQRM"*/, "-h", "target/html", "-u", "target/junit"
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-u", "junits")
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-h", "testresults")
  // ),
  scalacOptions ++= Seq(
    "-language:implicitConversions", "-explain",
    // See https://docs.scala-lang.org/scala3/guides/migration/tooling-syntax-rewriting.html
    "-no-indent",
    "-release:11",
  ),
  javacOptions := Seq("-source", "11", "-target", "11"),
)

// force redraft
// force redraft
