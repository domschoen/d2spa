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
      "name" -> EOValue(stringV = Some("Brunei Darussalam")),
      "alpha2_code" -> EOValue(stringV = Some("BN")),
      "alpha3_code" -> EOValue(stringV = Some("BRN"))
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

// case class EORef(entity: String, displayName: String, pk: Int)

  implicit val eoRefReads: Reads[EORef] = (
      (JsPath \ "entity").read[String] and
      (JsPath \ "displayName").read[String] and
      (JsPath \ "id").read[Int]
    ) (EORef.apply _)


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



  val fireRuleArguments = List("entity","task","propertyKey","key")

  def fireRuleFuture(entity: Option[String], task: Option[String], propertyKey: Option[String], key: String) : Future[WSResponse] = {
    val url = d2spaServerBaseUrl + "/fireRuleForKey.json";
    val fireRuleValues = List(entity,task,propertyKey,Some(key))
    val nonNullArguments = fireRuleArguments zip fireRuleValues
    val arguments = nonNullArguments.filter(x => !x._2.isEmpty).map(x => (x._1, x._2.get))

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

        PropertyMetaInfo(D2WContext(entity, task, null, propertyKey), List(
          RuleResult(propertyDisplayName._1, EOValueUtils.stringV(propertyDisplayName._2)),
          RuleResult(propertyComponentName._1,EOValueUtils.stringV(propertyComponentName._2))
          )
        )
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
    val displayPropertyKeysJson = response.json
    val array = displayPropertyKeysJson.asInstanceOf[JsArray]
    //System.out.println("--- " + task + " --- " + array)
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
      val entityDisplayNameFuture = fireRuleFuture(entity, TaskDefine.edit, RuleKeys.displayNameForEntity)
      val queryDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.query, RuleKeys.displayPropertyKeys)
      val editDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.edit, RuleKeys.displayPropertyKeys)
      val listDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.list, RuleKeys.displayPropertyKeys)
      val inspectDisplayPropertyKeysFuture = fireRuleFuture(entity, TaskDefine.inspect, RuleKeys.displayPropertyKeys)

      val result = for {
        r1 <- entityDisplayNameFuture
        queryDisplayPropertyKeys <- queryDisplayPropertyKeysFuture
        listDisplayPropertyKeys <- listDisplayPropertyKeysFuture
        inspectDisplayPropertyKeys <- inspectDisplayPropertyKeysFuture
        editDisplayPropertyKeys <- editDisplayPropertyKeysFuture
      } yield {
        val entityDisplayName = fromResponseToString(r1)

        val queryProperties = fromResponseToMetaInfo(queryDisplayPropertyKeys,entity,TaskDefine.query)
        val listProperties = fromResponseToMetaInfo(listDisplayPropertyKeys,entity,TaskDefine.list)
        val inspectProperties = fromResponseToMetaInfo(inspectDisplayPropertyKeys,entity,TaskDefine.inspect)
        val editProperties = fromResponseToMetaInfo(editDisplayPropertyKeys,entity,TaskDefine.edit)

        val emd = EntityMetaData(entity, entityDisplayName._2, Task(queryProperties), Task(listProperties),Task(inspectProperties),Task(editProperties))
        emd
      }
      result
    }
    else {
      if (entity.equals("Customer")) {
        Future(EntityMetaData("Customer", "Customer",
          Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "query", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "list", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName" ,EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "list", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "inspect", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "inspect", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "inspect", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "edit", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "edit", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            )
          )
        )
      } else
        Future(EntityMetaData("Project", "Project",
          Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "query", null, "descr"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Description")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(
                  D2WContext("Customer", "query", null, "projectNumber"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Project Number")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "list", null, "descr"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Description")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "inspect", null, "descr"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Description")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            ),
            Task(
              List(
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "descr"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Description")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WEditString"))
                  )),
                PropertyMetaInfo(D2WContext("Customer", "edit", null, "descr"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Description")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WEditString"))
                  ))
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
      f.map(response => {
          val jsObj = response.json.asInstanceOf[JsObject]
          val key = jsObj.keys.toSeq(0)

          println("Rule response: " + jsObj)

          val value = if (key.equals(RuleKeys.destinationEos)) {
            val rules = (jsObj \ RuleKeys.destinationEos).get.asInstanceOf[JsArray].value
            println("Rule response value: " + rules.getClass.getName)
            val eoRefJsSuccesses = rules.map (x => {
                println("x: " + x)

                x.validate[EORef]
              })
              val eoRefs = eoRefJsSuccesses.filter(x => x.isSuccess).map(_.get)
              EOValueUtils.eosV(eoRefs.toSeq)
          } else {
              EOValueUtils.stringV(jsObj.values.toSeq(0).asOpt[String].get)
          }
          RuleResult(key, value)
        }
      )
    }
    result
  }


  override def search(entity: String, queryValues: List[QueryValue]): Future[Seq[EO]] = {
    println("Search for entity: " + entity + " queryValues " + queryValues)
    //if (usesD2SPAServer) {
      searchOnD2SPAServer(entity, queryValues)
    //} else {
      // to be restored
      // searchOnOnlineCountryWs(qualifiers)
    //}
  }

  // TBD Sorting parameter
  // qualifier=product.name='CMS' and parentProductReleases.customer.acronym='ECHO'&sort=composedName|desc
  def qualifiersUrlPart(queryValues: List[QueryValue]) : String = {
    val qualifiersStrings = queryValues.map(qualifierUrlPart(_))
    return qualifiersStrings.mkString(" and ")
  }

  def qualifierUrlPart(queryValue: QueryValue) : String = {
    return "qualifier=" + queryValue.key + " like '*" + queryValue.value + "*'"
  }



  def searchOnD2SPAServer(entity: String, queryValues: List[QueryValue]): Future[Seq[EO]] = {
    val qualifierSuffix = if (queryValues == null || queryValues.isEmpty) "" else "?" + qualifiersUrlPart(queryValues)
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
        var valuesMap = Map[String, EOValue]()
        for ((key, value) <- obj.fields) {
          //println("value class " + value.getClass.getName)
          value match {
            case s: play.api.libs.json.JsString =>
              valuesMap += (key -> EOValue(stringV = Some(s.value)))
            case n: play.api.libs.json.JsNumber =>
              val bigDecimal = n.value
              // TBD use a BigDecimal container
              valuesMap += (key -> EOValue(stringV = Some(bigDecimal.toString())))
            case play.api.libs.json.JsNull =>
              // TBD use a kind of Null ?
              valuesMap += (key -> EOValue())
            case _ =>
              valuesMap += (key -> EOValue(stringV = Some("not supported")))

          }
        }
        eos ::= EO(entity,valuesMap)
      }
      eos.toSeq
    }
  }


  // http://www.groupkt.com/post/c9b0ccb9/country-and-other-related-rest-webservices.htm
/*  def searchOnOnlineCountryWs(qualifiers: List[EOKeyValueQualifier]): Future[Seq[EO]] = {
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
  }*/


  def woWsParameterForValue(value: EOValue): JsValue = {
    value.typeV match {
      case ValueType.stringV => JsString(value.stringV.get)
      case ValueType.eoV => JsObject( Seq( "pk attribute name" -> JsNumber(6), "type" -> JsString("entity name")))
      case _ => JsString("")
    }

  }

  def updateEO(entity: String, eo: EO): Future[EO] = {
    val url = d2spaServerBaseUrl + "/" + entity + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val eoDefinedValues = eo.values filter (v => { EOValueUtils.isDefined(v._2) })
    val eoValues = eoDefinedValues map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
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
        "descr" -> EOValue(stringV = Some("a")),
        "projectNumber" -> EOValue(stringV = Some("1"))
      )))

    } else {
      Future(EO(entity,Map (
        "name" -> EOValue(stringV = Some("a")),
        "acronym" -> EOValue(stringV = Some("1")),
        "address" -> EOValue(stringV = Some("Av. de France"))
      )))
    }
  }


}
