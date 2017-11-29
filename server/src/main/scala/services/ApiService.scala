package services

import java.util.{Date, UUID}

import d2spa.shared.{PropertyMetaInfo, _}
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

class ApiService(config: Configuration, ws: WSClient) extends Api {
  val usesD2SPAServer = config.getBoolean("d2spa.usesD2SPAServer").getOrElse(true)
  val d2spaServerBaseUrl = "http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra";


  var todos = Seq(
    TodoItem("41424344-4546-4748-494a-4b4c4d4e4f50", 0x61626364, "Wear shirt that says “Life”. Hand out lemons on street corner.", TodoLow, completed = false),
    TodoItem("2", 0x61626364, "Make vanilla pudding. Put in mayo jar. Eat in public.", TodoNormal, completed = false),
    TodoItem("3", 0x61626364, "Walk away slowly from an explosion without looking back.", TodoHigh, completed = false),
    TodoItem("4", 0x61626364, "Sneeze in front of the pope. Get blessed.", TodoNormal, completed = true)
  )

  var eos = Seq(
    EO("Project", Map(
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

          Menus(List(),D2WContext(null,null,null,null))
        } else {
          val firstChildEntity = mainMenus.head.children.head.entity
          Menus(mainMenus.toList,D2WContext(firstChildEntity,"query",null,null))
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
        D2WContext("Project", "query", null, null)
      )
      Future(data)
    }
  }


  def fireRuleFuture(d2WContext: D2WContext, key: String) : Future[WSResponse] = {
    val entityOpt = if (d2WContext.entity == null) None else Some(d2WContext.entity)
    val taskOpt = if (d2WContext.task == null) None else Some(d2WContext.task)
    val propertyKeyOpt = if (d2WContext.propertyKey == null) None else Some(d2WContext.propertyKey)
    fireRuleFuture(entityOpt, taskOpt,propertyKeyOpt,key)
  }

  def fireRuleFuture(entity: String, task: String, key: String) : Future[WSResponse] = {
    fireRuleFuture(Some(entity),Some(task),None,key)
  }
  def fireRuleFuture(entity: String, task: String, propertyKey: String, key: String) : Future[WSResponse] = {
    fireRuleFuture(Some(entity),Some(task),Some(propertyKey),key)
  }



  val fireRuleArguments = List("entity","task","propertyKey")

  def fireRuleFuture(entity: Option[String], task: Option[String], propertyKey: Option[String], key: String) : Future[WSResponse] = {
    val url = d2spaServerBaseUrl + "/fireRuleForKey.json";
    val fireRuleValues = List(entity,task,propertyKey)
    val nonNullArguments = fireRuleArguments zip fireRuleValues
    val arguments = nonNullArguments.filter(x => !x._2.isEmpty).map(x => (x._1, x._2.get)) :+ ("key",key)

    val request: WSRequest = ws.url(url)
      .withQueryString(arguments.toArray: _*)
      .withRequestTimeout(10000.millis)
    request.get()
  }


  private def lift[T](futures: Seq[Future[T]]) =
    futures.map(_.map { Success(_) }.recover { case t => Failure(t) })


  def waitAll[T](futures: Seq[Future[T]]) =
    Future.sequence(lift(futures)) // having neutralized exception completions through the lifting, .sequence can now be used


  def propertyMetaInfosForTask(displayPropertyKeys: JsArray, entity: String, task: String) = {
    val propertiesFutures = displayPropertyKeys.value.map( x => {
      val propertyKey = x.asOpt[String].get
      val propertyDisplayNameFuture = fireRuleFuture(entity, task, propertyKey, "displayNameForProperty")
      val componentNameFuture = fireRuleFuture(entity, task, propertyKey, "componentName")

      val subResult = for {
        pDisplayName <- propertyDisplayNameFuture
        pComponentName <- componentNameFuture
      } yield {
        val propertyDisplayName = fromResponseToString(pDisplayName)
        val propertyComponentName = fromResponseToString(pComponentName)

        println("<" + propertyComponentName + ">")

        PropertyMetaInfo(D2WContext(entity, task, null, propertyKey) ,StringValue(""),
          List(RuleResult(propertyDisplayName._1,propertyDisplayName._2), RuleResult(propertyComponentName._1,propertyComponentName._2)))
      }
      subResult
    }).toList
    val futureOfList = Future sequence propertiesFutures
    val properties = Await result (futureOfList, 2 seconds)
    properties
  }

  def fromResponseToString(response: WSResponse) = {
    val jsObj = response.json.asInstanceOf[JsObject]
    (jsObj.keys.toSeq(0), jsObj.values.toSeq(0).asOpt[String].get)
  }


  def fromResponseToMetaInfo(response: WSResponse, entity: String, task: String) = {
    val queryDisplayPropertyKeysJson = response.json
    val array = queryDisplayPropertyKeysJson.asInstanceOf[JsArray]
    propertyMetaInfosForTask(array, entity, task)
  }

  // To create the result, it needs to issue multiple query on the D2W rule system
  // 1) get entity display name for edit, inspect, query, list
  // D2WContext: task=edit, entity=..
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayNameForEntity
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayPropertyKeys
  def getMetaData(entity: String): Future[EntityMetaData] = {
    val finished = true //usesD2SPAServer
    if (finished) {
      val entityDisplayNameFuture = fireRuleFuture(entity, TaskDefine.edit, "displayNameForEntity")
      val queryDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.query, "displayPropertyKeys")
      val editDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.edit, "displayPropertyKeys")
      val listDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.list, "displayPropertyKeys")
      val inspectDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.inspect, "displayPropertyKeys")

      val result = for {
        r1 <- entityDisplayNameFuture
        r2 <- queryDisplayPropertyKeysFuture
        r3 <- listDisplayPropertyKeysFuture
        r4 <- inspectDisplayPropertyKeysFuture
        r5 <- editDisplayPropertyKeysFuture
      } yield {
        val entityDisplayName = fromResponseToString(r1)

        val queryProperties = fromResponseToMetaInfo(r2,entity,TaskDefine.query)
        val listProperties = fromResponseToMetaInfo(r2,entity,TaskDefine.list)
        val inspectProperties = fromResponseToMetaInfo(r2,entity,TaskDefine.inspect)
        val editProperties = fromResponseToMetaInfo(r2,entity,TaskDefine.edit)


        val emd = EntityMetaData(entity, entityDisplayName._2, Task(queryProperties), Task(listProperties),Task(inspectProperties),Task(editProperties))
        println("emd" + emd)
        emd
      }
      result
    }
    else {
      if (entity.equals("Customer")) {
        Future(EntityMetaData("Customer", "Customer",
          Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "query", null, "name"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Name"), RuleResult("componentName","ERD2WQueryStringOperator"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "acronym"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Acronym"), RuleResult("componentName","ERD2WQueryStringOperator"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "address"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Address"), RuleResult("componentName","ERD2WQueryStringOperator")))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "list", null, "name"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Name"), RuleResult("componentName" ,"ERD2WDisplayString"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "list", null, "acronym"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Acronym"), RuleResult("componentName","ERD2WDisplayString")))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "inspect", null, "name"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Name"), RuleResult("componentName","ERD2WDisplayString"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "inspect", null, "acronym"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Acronym"), RuleResult("componentName","ERD2WDisplayString"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "inspect", null, "address"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Address"), RuleResult("componentName","ERD2WDisplayString")))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "name"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Name"), RuleResult("componentName","ERD2WQueryStringOperator"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "edit", null, "acronym"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Acronym"), RuleResult("componentName","ERD2WQueryStringOperator"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "edit", null, "address"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Address"), RuleResult("componentName","ERD2WQueryStringOperator")))
              )
            )
          )
        )
      } else
        Future(EntityMetaData("Project", "Project",
          Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "query", null, "descr"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Description"), RuleResult("componentName","ERD2WQueryStringOperator"))),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "projectNumber"),
                  StringValue(""),
                  List(RuleResult("displayNameForProperty","Project Number"), RuleResult("componentName","ERD2WQueryStringOperator")))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "list", null, "descr"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Description"), RuleResult("componentName","ERD2WQueryStringOperator")))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "inspect", null, "descr"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Description"), RuleResult("componentName","ERD2WQueryStringOperator")))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "descr"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Description"), RuleResult("componentName","ERD2WEditString"))),
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "descr"), StringValue(""),
                  List(RuleResult("displayNameForProperty","Description"), RuleResult("componentName","ERD2WEditString")))
              )
            )
          )
        )
    }
  }
  def fireRules(d2WContext: D2WContext, keysToFire: List[String]): Future[List[RuleResult]] = {
    val futures = keysToFire.map(x => {fireRuleFuture(d2WContext, x)})
    val futureSequece = Future sequence futures
    val result = for {
      f <- futureSequece
    } yield {
      f.map(x => { val tuple = fromResponseToString(x); RuleResult(tuple._1,tuple._2)})
    }
    result
  }


  override def search(entity: String, qualifiers: List[EOKeyValueQualifier]): Future[Seq[EO]] = {
    println("Search for entity: " + entity + " qualifiers " + qualifiers)
    if (usesD2SPAServer) {
      searchOnD2SPAServer(entity, qualifiers)
    } else {
      searchOnOnlineCountryWs(qualifiers)
    }
  }

  // TBD Sorting parameter
  // qualifier=product.name='CMS' and parentProductReleases.customer.acronym='ECHO'&sort=composedName|desc
  def qualifiersUrlPart(qualifiers: List[EOKeyValueQualifier]) : String = {
    val qualifiersStrings = qualifiers.map(qualifierUrlPart(_))
    return qualifiersStrings.mkString(" and ")
  }

  def qualifierUrlPart(qualifier: EOKeyValueQualifier) : String = {
    return "qualifier=" + qualifier.key + " like '*" + qualifier.value + "*'"
  }


  def searchOnD2SPAServer(entity: String, qualifiers: List[EOKeyValueQualifier]): Future[Seq[EO]] = {
    val qualifierSuffix = if (qualifiers == null || qualifiers.isEmpty) "" else "?" + qualifiersUrlPart(qualifiers)
    val url = d2spaServerBaseUrl + "/" + entity + ".json" + qualifierSuffix
    println("Search URL:" + url)
    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      val array = resultBody.asInstanceOf[JsArray]
      var eos = List[EO]()
      for (item <- array.value) {
        val obj = item.asInstanceOf[JsObject]
        var valuesMap = Map[String, StringValue]()
        for ((key, value) <- obj.fields) {
          //println("value class " + value.getClass.getName)
          value match {
            case s: play.api.libs.json.JsString =>
              valuesMap += (key -> StringValue(s.value))
            case n: play.api.libs.json.JsNumber =>
              val bigDecimal = n.value
              // TBD use a BigDecimal container
              valuesMap += (key -> StringValue(bigDecimal.toString()))
            case play.api.libs.json.JsNull =>
              // TBD use a kind of Null ?
              valuesMap += (key -> StringValue(""))
            case _ =>
              valuesMap += (key -> StringValue("not supported"))

          }
        }
        eos ::= EO(entity,valuesMap)
      }
      eos.toSeq
    }
  }


  // http://www.groupkt.com/post/c9b0ccb9/country-and-other-related-rest-webservices.htm
  def searchOnOnlineCountryWs(qualifiers: List[EOKeyValueQualifier]): Future[Seq[EO]] = {
    val url = "http://services.groupkt.com/country/search?text=" + qualifiers.head.value;
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

            eos ::= EO("Project", Map(
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

  // New EO has to be created by the server because it needs to contain all attribute even those not directly displayed
  def newEO(entity:String) : Future[EO] = {
    println("New EO for entity: " + entity)
    if (entity.equals("Project")) {
      Future(EO(entity,Map (
        "descr" -> StringValue("a"),
        "projectNumber" -> StringValue("1")
      )))

    } else {
      Future(EO(entity,Map (
        "name" -> StringValue("a"),
        "acronym" -> StringValue("1"),
        "address" -> StringValue("Av. de France")
      )))
    }
  }


}
