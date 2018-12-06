package services

import java.util.{Date, UUID}

import com.fasterxml.jackson.core.JsonParseException
import d2spa.shared
import d2spa.shared.{RuleResult, _}
import play.api.Configuration
import play.api.libs.ws._
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.WSAuthScheme
import play.api.libs.ws._

import scala.concurrent.Future
import scala.concurrent.Await
import scala.util._
import scala.concurrent.duration._
import play.api.libs.json.{JsString, _}
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.Logger

case class CountryItem(
                        name: String,
                        alpha2_code: String,
                        alpha3_code: String
                      )












class ApiService(config: Configuration, ws: WSClient) extends Api {
  val showDebugButton = config.getBoolean("d2spa.showDebugButton").getOrElse(true)
  val d2spaServerBaseUrl = config.getString("d2spa.woappURL").getOrElse("http://localhost:1445/cgi-bin/WebObjects/CustomerBackend.woa/ra")


  val timeout = 50.seconds





}
