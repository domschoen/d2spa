package services

import java.util.{Date, UUID}

import d2spa.shared._
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
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._


case class CountryItem(
                        name : String,
                        alpha2_code: String,
                        alpha3_code: String
                      )

case class MenuItem (
                        id : Int,
                        `type`: String,
                        title: String,
                        entity: Option[String],
                        parent: Option[MenuItem]
                      )

class ApiService(config: Configuration) extends Api {
  val usesD2SPAServer = config.getBoolean("usesD2SPAServer").getOrElse(true)
  val d2spaServerBaseUrl = "http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra";


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

  //def wsGetSync(url: String): Any = Await.result(WS.url(url).get(), scala.concurrent.duration.Duration(120000, MILLISECONDS))


  implicit val CountryReads: Reads[CountryItem] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "alpha2_code").read[String] and
      (JsPath \ "alpha3_code").read[String]
    )(CountryItem.apply _)

  implicit lazy val menuItemReads: Reads[MenuItem] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "type").read[String] and
      (JsPath \ "title").read[String] and
      (JsPath \ "entity").readNullable[String] and
      (JsPath \ "parent").lazyReadNullable(menuItemReads)
    )(MenuItem.apply _)


  override def getMenus(): Future[Menus] = {
    println("get Menus")

    if (usesD2SPAServer) {
      val url = d2spaServerBaseUrl + "/Menu.json";
      val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
      val futureResponse: Future[WSResponse] = request.get()
      futureResponse.map { response =>
        val resultBody = response.json
        val array = resultBody.asInstanceOf[JsArray]
        var menus = List[MenuItem]()
        for (menuRaw <- array.value) {
          //println(menuRaw)
          val obj = menuRaw.validate[MenuItem]
          obj match {
            case s: JsSuccess[MenuItem] => {
              val wiObj = s.get
              menus = wiObj :: menus
            }
            case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
          }
        }
        val children = menus.filter(_.parent.isDefined)
        val childrenByParent = children.groupBy(_.parent.get)

        val mainMenus = childrenByParent.map{
          case (mm, cs) =>
            val childMenus = cs.map(cm => Menu(cm.id,cm.title,cm.entity.getOrElse(null)))
            MainMenu(mm.id,mm.title,childMenus)
        }
        if (mainMenus.isEmpty) {
          // TOTO Menus containing D2WContext is not a good choice because better to have
          // None D2WContext if no menus (at least, D2WContext should be an option instead of returning
          // D2WContext(null,null,null)

          Menus(List(),D2WContext(null,null,null))
        } else {
          val firstChildEntity = mainMenus.head.children.head.entity
          Menus(mainMenus.toList,D2WContext(firstChildEntity,"query",null))
        }
      }

    } else {

      val data = Menus(
        List(
          MainMenu(1, "Project Management",
            List(
              Menu(2, "Project", "Project"),
              Menu(3, "Customer", "Customer")
            )
          )
        ),
        D2WContext("Project", "query", null)
      )
      Future(data)
    }
  }

  def getMetaData(): MetaDatas =
    MetaDatas(
      List(
        EntityMetaData("Customer", "Customer",
          QueryTask(
            List(
              QueryProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
              QueryProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
            )
          ),
          ListTask(
            List(
              ListProperty("name", "Name","ERD2WQueryStringOperator"),
              ListProperty("operator", "Operator","ERD2WQueryStringOperator")
            )
          ),
          InspectTask(
            List(
              EditInspectProperty("name", "Name","ERD2WQueryStringOperator"),
              EditInspectProperty("operator", "Operator","ERD2WQueryStringOperator")
            )
          ),
          EditTask(
            List(
              EditInspectProperty("name", "Name","ERD2WQueryStringOperator"),
              EditInspectProperty("operator", "Operator","ERD2WQueryStringOperator")
            )
          )
        ),
        EntityMetaData("Project", "Project",
          QueryTask(
            List(
              QueryProperty("name", "Name","ERD2WQueryStringOperator",StringValue("fr"))//,
              //QueryProperty("csad", "CSAD","ERD2WQueryStringOperator",StringValue("toto"))
            )
          ),
          ListTask(
            List(
              ListProperty("name", "Name","ERD2WQueryStringOperator")//,
              //ListProperty("csad", "CSAD","ERD2WQueryStringOperator")
            )
          ),
          InspectTask(
            List(
              EditInspectProperty("name", "Name","ERD2WQueryStringOperator")
            )
          ),
          EditTask(
            List(
              EditInspectProperty("name", "Name","ERD2WQueryStringOperator"),
              EditInspectProperty("operator", "Operator","ERD2WQueryStringOperator")
            )
          )
        )
      )
    )

  override def search(entity: String, qualifier: EOKeyValueQualifier): Future[Seq[EO]] = {
    if (usesD2SPAServer) {
      searchOnD2SPAServer(entity, qualifier)
    } else {
      searchOnOnlineCountryWs(qualifier)
    }
  }

  def qualifierUrlPart(qualifier: EOKeyValueQualifier) : String = {
    return "qualifier=" + qualifier.key + " like '*" + qualifier.value + "*'"
  }


  def searchOnD2SPAServer(entity: String, qualifier: EOKeyValueQualifier): Future[Seq[EO]] = {
    val qualifierSuffix = if (qualifier == null) "" else "?" + qualifierUrlPart(qualifier)
    val url = d2spaServerBaseUrl + "/" + entity + ".json" + qualifierSuffix

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      val array = resultBody.asInstanceOf[JsArray]
      var eos = List[EO]()
      for (item <- array.value) {

        println("item " + item)
        //val obj = country.validate[CountryItem]
        /*obj match {
          case s: JsSuccess[CountryItem] => {
            val wiObj = s.get

            eos ::= EO(Map(
              "name" -> StringValue(wiObj.name),
              "alpha2_code" -> StringValue(wiObj.alpha2_code),
              "alpha3_code" -> StringValue(wiObj.alpha3_code)))
          }
          case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
        }*/
      }
      //eos.toSeq
      Seq()
    }
  }


  // http://www.groupkt.com/post/c9b0ccb9/country-and-other-related-rest-webservices.htm
  def searchOnOnlineCountryWs(qualifier: EOKeyValueQualifier): Future[Seq[EO]] = {
    val url = "http://services.groupkt.com/country/search?text=" + qualifier.value;
    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

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

  def updateEO(entity: String, eo: EO): Future[EO] = {
    val url = d2spaServerBaseUrl + "/" + entity + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val eoValues = eo.values map {case (key,valContainer) => (key, valContainer.value)}
    val data = Json.toJson(eoValues)


    val futureResponse: Future[WSResponse] = request.post(data)
    futureResponse.map { response =>

      val resultBody = response.json
      val array = resultBody.asInstanceOf[JsObject]
      eo
    }
  }

}
