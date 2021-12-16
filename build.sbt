name := "LowChess"

version := "0.1.0-SNAPSHOT"

scalaVersion := "3.0.2"

val http4sVersion = "1.0.0-M23"
val doobieVersion = "1.0.0-RC1"
val circeVersion = "0.14.1"
libraryDependencies ++= Seq(
  "org.http4s"            %% "http4s-dsl"          % http4sVersion,
  "org.http4s"            %% "http4s-core"         % http4sVersion,
  "org.http4s"            %% "http4s-circe"        % http4sVersion,
  "org.http4s"            %% "http4s-blaze-server" % http4sVersion,
  "com.github.pureconfig" %% "pureconfig-core"     % "0.17.1",
  "org.postgresql"         % "postgresql"          % "42.3.1",
  "org.flywaydb"           % "flyway-core"         % "8.1.0",
  "org.tpolecat"          %% "doobie-core"         % doobieVersion,
  "org.tpolecat"          %% "doobie-postgres"     % doobieVersion,
  "io.circe"              %% "circe-core"          % circeVersion,
  "io.circe"              %% "circe-parser"        % circeVersion,
  "com.google.zxing"       % "core"                % "3.4.1",
  "ch.qos.logback"         % "logback-classic"     % "1.2.8",
  "org.typelevel"         %% "log4cats-slf4j"      % "2.1.1",
  "org.scalatest"         %% "scalatest"           % "3.2.10" % "test"
)

scalacOptions += "-source:future" //Enable better monadic for syntax