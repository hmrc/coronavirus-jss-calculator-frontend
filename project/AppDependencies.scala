import sbt._

object AppDependencies {
  import play.core.PlayVersion

  val compile = Seq(
    play.sbt.PlayImport.ws,
    "org.reactivemongo" %% "play2-reactivemongo"            % "0.18.6-play26",
    "uk.gov.hmrc"       %% "logback-json-logger"            % "4.8.0",
    "uk.gov.hmrc"       %% "play-health"                    % "3.15.0-play-26",
    "uk.gov.hmrc"       %% "play-conditional-form-mapping"  % "1.2.0-play-26",
    "uk.gov.hmrc"       %% "bootstrap-play-26"              % "1.16.0",
    "uk.gov.hmrc"       %% "play-whitelist-filter"          % "3.2.0-play-26",
    "uk.gov.hmrc"       %% "play-frontend-govuk"            % "0.49.0-play-26",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"             % "0.17.0-play-26",
    "uk.gov.hmrc"       %% "domain"                         % "5.6.0-play-26"
  )

  val test = Seq(
    "org.scalatest"               %% "scalatest"           % "3.0.8",
    "org.scalatestplus.play"      %% "scalatestplus-play"  % "3.1.2",
    "org.pegdown"                 %  "pegdown"             % "1.6.0",
    "org.jsoup"                   %  "jsoup"               % "1.12.1",
    "com.typesafe.play"           %% "play-test"           % PlayVersion.current,
    "org.mockito"                 %  "mockito-all"         % "1.10.19",
    "org.scalacheck"              %% "scalacheck"          % "1.14.1",
    "com.github.tomakehurst"      %  "wiremock-standalone" % "2.25.0"
  ).map(_ % "test,it")

  def apply(): Seq[ModuleID] = compile ++ test

  val akkaVersion = "2.5.23"
  val akkaHttpVersion = "10.0.15"

  val overrides = Seq(
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-protobuf" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
  )
}
