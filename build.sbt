val zioVersion            = "2.0.19"
val zioJsonVersion        = "0.6.2"
val zioConfigVersion      = "3.0.7"

val root = (project in file("."))
  .settings(
    inThisBuild(
      List(
        organization := "com.example",
        scalaVersion := "2.13.16"
        //scalaVersion := "2.13.0"
      )
    ),
    name           := "solid-practice",
    libraryDependencies ++= Seq(
      // general

      "dev.zio"        %% "zio"                 % zioVersion,

      // test
      "dev.zio"            %% "zio-test-sbt"                    % zioVersion            % Test,
      "org.scalactic" %% "scalactic" % "3.2.19",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",

    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )
  .enablePlugins(DockerPlugin, JavaAppPackaging)
