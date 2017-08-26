package services

import java.util.{UUID, Date}

import d2spa.shared._
import play.api.libs.ws._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current
import play.api.libs.ws.WSAuthScheme
import play.api.libs.ws._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.util._
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._


case class CountryItem(
                        name : String,
                        alpha2_code: String,
                        alpha3_code: String
                                 )


class ApiService extends Api {
  var todos = Seq(
    TodoItem("41424344-4546-4748-494a-4b4c4d4e4f50", 0x61626364, "Wear shirt that says “Life”. Hand out lemons on street corner.", TodoLow, completed = false),
    TodoItem("2", 0x61626364, "Make vanilla pudding. Put in mayo jar. Eat in public.", TodoNormal, completed = false),
    TodoItem("3", 0x61626364, "Walk away slowly from an explosion without looking back.", TodoHigh, completed = false),
    TodoItem("4", 0x61626364, "Sneeze in front of the pope. Get blessed.", TodoNormal, completed = true)
  )

  var eos = Seq(
    EO(Map(
      "name" -> StringValue("Brunei Darussalam"),
      "alpha2_code" -> StringValue("BN"),
      "alpha3_code" -> StringValue("BRN")
      )
    )
  )

  val safeUrl = "http://galactica.hq.k.grp:1445/cgi-bin/WebObjects/D2SPAServer.woa/ra/User?qualifier=firstname='Mike'"

  def wsGetSync(url: String): Any = Await.result(WS.url(url).get(), scala.concurrent.duration.Duration(120000, MILLISECONDS))

  override def welcomeMsg(name: String): String = {
    val result = wsGetSync(safeUrl)
    println(result)
    s"Welcome to SPA, $name! Time is now ${new Date}"
  }

  implicit val CountryReads: Reads[CountryItem] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "alpha2_code").read[String] and
      (JsPath \ "alpha3_code").read[String]
    )(CountryItem.apply _)


  override def search(qualifier: EOKeyValueQualifier): Seq[EO] = {
    searchOnOnlineCountryWs(qualifier)
  }


  override def getMenus(): Menus = {
    println("get Menus")
    Menus(
      List(
        MainMenu(1, "MCU",
          List(
            Menu(2, "Nagra MCU", "DTEChipset"),
            Menu(3, "EMI", "DTEEMI"),
            Menu(4, "Chipset Security Type", "ChipsetSecurityType")
          )
        )
      ),
      D2WContext("DTEEMI", "query", null)
    )
  }


  // http://www.groupkt.com/post/c9b0ccb9/country-and-other-related-rest-webservices.htm
  def searchOnOnlineCountryWs(qualifier: EOKeyValueQualifier): Seq[EO] = {
    val url = "http://services.groupkt.com/country/search?text=" + qualifier.value;
    val response = wsGetSync(url).asInstanceOf[WSResponse]
    val resultBody = response.json \ "RestResponse" \ "result"
    val array = resultBody.asInstanceOf[JsDefined].value.asInstanceOf[JsArray]
    var eos = List[EO]()
    for (country <- array.value) {
      val obj = country.validate[CountryItem]
      obj match {
        case s: JsSuccess[CountryItem] => {
          val wiObj = s.get

          eos ::= EO(Map(
            "name" -> StringValue(wiObj.name),
            "alpha2_code" -> StringValue(wiObj.alpha2_code),
            "alpha3_code" -> StringValue(wiObj.alpha3_code)))
        }
        case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
      }
    }
    eos.toSeq
  }


}
