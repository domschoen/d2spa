import sbtcrossproject.{crossProject, CrossType}
import sbt.Keys._
import sbt.Project.projectToRef


// instantiate the JVM project for SBT with some additional settings
lazy val server = (project in file("server"))
  .settings(
    name := "server",
    scalaJSProjects := Seq(client),
    version := Settings.version,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= Settings.scalacOptions,
    routesGenerator := InjectedRoutesGenerator,
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(digest, gzip),
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
    libraryDependencies ++= Seq(
        ws,
        guice,
        specs2 % Test
    ) ++ Settings.jvmDependencies.value,
     EclipseKeys.preTasks := Seq(compile in Compile),
     LessKeys.compress in Assets := true
  )
  .enablePlugins(PlayScala)
  .aggregate(clients.map(projectToRef): _*) // This allow to run all tests (frontend and backend with "test"
  .dependsOn(sharedJvm)

lazy val clients = Seq(client)


// instantiate the JS project for SBT with some additional settings
lazy val client = (project in file("client")).settings(commonSettings)
  .settings(
    name := "client",
    // use Scala.js provided launcher code to start the client app
    scalaJSUseMainModuleInitializer := true,
    // scalaJSUseMainModuleInitializer in Test := false,
    scalacOptions ++= Seq(
        "-Xlint",
        "-unchecked",
        "-deprecation",
        "-feature"
    ),
    version := Settings.version,
    scalaVersion := Settings.versions.scala,
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.6",
        "com.github.japgolly.scalajs-react" %%% "core" % "1.3.1",
        "com.github.japgolly.scalajs-react" %%% "extra" % "1.3.1",
        "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3",
        //"com.typesafe.play" %%% "play-json" % "2.6.10",
        "io.suzaku" %%% "diode" % "1.1.4",
        "io.suzaku" %%% "diode-react" % "1.1.4.131",
        "com.zoepepper" %%% "scalajs-jsjoda" % "1.1.1",
        "com.zoepepper" %%% "scalajs-jsjoda-as-java-time" % "1.1.1",
        "com.lihaoyi" %%% "utest" % "0.6.5" % Test,
        "com.lihaoyi" %%% "autowire" % Settings.versions.autowire,
        "io.suzaku" %%% "boopickle" % Settings.versions.booPickle,
        "org.scala-js" %%% "scalajs-java-logging" % "0.1.5"
    ),
    dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2",
    jsDependencies ++= Seq(
        "org.webjars.npm" % "react" % "16.5.1" / "umd/react.development.js" minified "umd/react.production.min.js" commonJSName "React",
        "org.webjars.npm" % "react-dom" % "16.5.1" / "umd/react-dom.development.js" minified "umd/react-dom.production.min.js" dependsOn "umd/react.development.js" commonJSName "ReactDOM",
        "org.webjars.npm" % "react-dom" % "16.5.1" / "umd/react-dom-server.browser.development.js" minified  "umd/react-dom-server.browser.production.min.js" dependsOn "umd/react-dom.development.js" commonJSName "ReactDOMServer",
        "org.webjars" % "jquery" % "1.11.1" / "jquery.js" minified "jquery.min.js",
        "org.webjars" % "bootstrap" % "3.3.6" / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js",
        "org.webjars" % "log4javascript" % "1.4.10" / "js/log4javascript_uncompressed.js" minified "js/log4javascript.js",
        "org.webjars.npm" % "js-joda" % "1.1.8" / "dist/js-joda.js" minified "dist/js-joda.min.js"
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv,
    // by default we do development build, no eliding
    //elideOptions := Seq("-Xelide-below", "WARNING"),
    //elideOptions := Seq(),
    //elideOptions := Seq("-Xelide-below", sys.props.getOrElse("elide.below", "0")),
    //scalacOptions ++= elideOptions.value,
    // RuntimeDOM is needed for tests
    // jsDependencies += RuntimeDOM % "test",
    // yes, we want to package JS dependencies
    // skip in packageJSDependencies := false,
    // use uTest framework for tests
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .dependsOn(sharedJs)



lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(commonSettings)
  .jvmSettings(
    libraryDependencies ++= Seq(
        "com.lihaoyi" %% "autowire" % Settings.versions.autowire,
        "io.suzaku" %% "boopickle" % Settings.versions.booPickle
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
        "com.zoepepper" %%% "scalajs-jsjoda" % "1.1.1",
        "com.zoepepper" %%% "scalajs-jsjoda-as-java-time" % "1.1.1",
        "com.lihaoyi" %%% "autowire" % Settings.versions.autowire,
        "io.suzaku" %%% "boopickle" % Settings.versions.booPickle,
        "org.scala-js" %%% "scalajs-java-logging" % "0.1.5"
    )
  )


lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val commonSettings = Seq(
  scalaVersion := "2.12.6"
)

// use eliding to drop some debug code in the production build
// lazy val elideOptions = settingKey[Seq[String]]("Set limit for elidable functions")


// loads the Play server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen {s: State => "project server" :: s}
