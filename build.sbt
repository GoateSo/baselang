val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "baselang",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit"     % "0.7.29" % Test,
    libraryDependencies += "com.lihaoyi"   %% "fastparse" % "3.1.0",
    libraryDependencies += "com.lihaoyi"   %% "pprint"    % "0.9.0",
    libraryDependencies += "com.lihaoyi"   %% "os-lib"    % "0.10.1"
  )
