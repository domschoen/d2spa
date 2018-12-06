import sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/**
 * Application settings. Configure the build for your application here.
 * You normally don't have to touch the actual build definition after this.
 */
object Settings {
  /** The name of your application */
  val name = "scalajs-spa"



  /** The version of your application */
  val version = "1.1.5"

  /** Options for the scala compiler */
  val scalacOptions = Seq(
    "-Xlint",
    "-unchecked",
    "-deprecation",
    "-feature"
  )

  /** Declare global dependency versions here to avoid mismatches in multi part dependencies */
  object versions {
    val scala = "2.12.6"
    val scalaDom = "0.9.6"
    val scalajsReact = "1.1.0"
    val scalaCSS = "0.5.3"
    val log4js = "1.4.10"
    val autowire = "0.2.6"
    val booPickle = "1.3.0"
    val diode = "1.1.4"
    val diodeReact = "1.1.4.131"
    val uTest = "0.4.7"
    val fontawesome = "4.3.0-1"
    val kryo = "2.24.0"

    val react = "16.5.1"
    val jQuery = "1.11.1"
    //val bootstrap = "4.0.0-beta"
    //val bootstrap = "3.3.7" : doesn't compile less files not found
    val bootstrap = "3.3.6"
    val chartjs = "2.1.3"

    val scalajsScripts = "1.1.2"
    val scalatestplusplay = "3.1.2"
  }



  /** Dependencies only used by the JVM project */
  val jvmDependencies = Def.setting(Seq(
    "com.vmunier" %% "scalajs-scripts" % versions.scalajsScripts,
    "org.scalatestplus.play" %% "scalatestplus-play" % versions.scalatestplusplay % Test,
    "org.webjars" % "font-awesome" % versions.fontawesome % Provided,
    "org.webjars" % "bootstrap" % versions.bootstrap % Provided,
    "com.esotericsoftware.kryo" % "kryo" % versions.kryo,
    "com.lihaoyi" %% "utest" % versions.uTest % Test,
    "com.lihaoyi" %% "autowire" % versions.autowire,
    "io.suzaku" %% "boopickle" % versions.booPickle,
    "com.github.japgolly.scalacss" %% "core" % versions.scalaCSS
  ))



  /** Dependencies for external JS libs that are bundled into a single .js file according to dependency order
      "org.webjars.bower" % "popper.js" % "1.12.5" / "popper.js" minified "popper.min.js",
 */
  val jsDependencies = Def.setting(Seq(
    "org.webjars.npm" % "react" % versions.react / "umd/react.development.js" minified "umd/react.production.min.js" commonJSName "React",
    "org.webjars.npm" % "react-dom" % versions.react / "umd/react-dom.development.js" minified "umd/react-dom.production.min.js" dependsOn "umd/react.development.js" commonJSName "ReactDOM",
    "org.webjars.npm" % "react-dom" % versions.react / "umd/react-dom-server.browser.development.js" minified  "umd/react-dom-server.browser.production.min.js" dependsOn "umd/react-dom.development.js" commonJSName "ReactDOMServer",
    "org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js",
    "org.webjars" % "chartjs" % versions.chartjs / "Chart.js" minified "Chart.min.js",
    "org.webjars" % "log4javascript" % versions.log4js / "js/log4javascript_uncompressed.js" minified "js/log4javascript.js",
    "org.webjars.npm" % "js-joda" % "1.1.8" / "dist/js-joda.js" minified "dist/js-joda.min.js"
  ))
}
