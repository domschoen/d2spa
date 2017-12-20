package services

import java.util.{Date, UUID}

import com.fasterxml.jackson.core.JsonParseException
import d2spa.shared
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

case class EntityItem (
                      name: String,
                      pkAttributeName: String
                    )


case class MenuItem (
                        id : Int,
                        `type`: String,
                        title: String,
                        entity: EntityItem,
                        parent: Option[MenuItem]
                      )



class ApiService(config: Configuration, ws: WSClient) extends Api {
  val usesD2SPAServer = config.getBoolean("d2spa.usesD2SPAServer").getOrElse(true)
  val showDebugButton = config.getBoolean("d2spa.showDebugButton").getOrElse(true)
  val d2spaServerBaseUrl = "http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra";


  var eos = Seq(
    EO(EOEntity("Project","id"), Map(
      "name" -> EOValue(stringV = Some("Brunei Darussalam")),
      "alpha2_code" -> EOValue(stringV = Some("BN")),
      "alpha3_code" -> EOValue(stringV = Some("BRN"))
      ),None
    )
  )

  val safeUrl = "http://galactica.hq.k.grp:1445/cgi-bin/WebObjects/D2SPAServer.woa/ra/User?qualifier=firstname='Mike'"

  //def wsGetSync(url: String): Any = Await.result(WS.url(url).get(), scala.concurrent.duration.Duration(120000, MILLISECONDS))


  implicit val CountryReads: Reads[CountryItem] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "alpha2_code").read[String] and
      (JsPath \ "alpha3_code").read[String]
    )(CountryItem.apply _)

  implicit lazy val entityItemReads: Reads[EntityItem] = (
      (JsPath \ "name").read[String] and
      (JsPath \ "pkAttributeName").read[String]
    )(EntityItem.apply _)


  implicit lazy val menuItemReads: Reads[MenuItem] = (
      (JsPath \ "id").read[Int] and
      (JsPath \ "type").read[String] and
      (JsPath \ "title").read[String] and
      ((JsPath \ "entity").read[EntityItem] orElse(Reads.pure(null))) and
      (JsPath \ "parent").lazyReadNullable(menuItemReads)
    )(MenuItem.apply _)

// case class EORef(entity: String, displayName: String, pk: Int)

  implicit val eoRefReads: Reads[EORef] = (
      (JsPath \ "entity").read[String] and
      (JsPath \ "displayName").read[String] and
      (JsPath \ "id").read[Int] and
      (JsPath \ "pkAttributeName").read[String]
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
            val childMenus = cs.map(cm => Menu(cm.id,cm.title,EOEntity(cm.entity.name, cm.entity.pkAttributeName)))
            MainMenu(mm.id,mm.title,childMenus)
        }
        if (mainMenus.isEmpty) {
          // TOTO Menus containing D2WContext is not a good choice because better to have
          // None D2WContext if no menus (at least, D2WContext should be an option instead of returning
          // D2WContext(null,null,null)

          Menus(List(),D2WContext(null,null,null,null),showDebugButton)
        } else {
          val firstChildEntity = mainMenus.head.children.head.entity
          Menus(mainMenus.toList,D2WContext(firstChildEntity,"query",null,null),showDebugButton)
        }
      }

    } else {

      val data = Menus(
        List(
          MainMenu(1, "Project Management",
            List(
              Menu(2, "Project", EOEntity("Project","id")),
              Menu(3, "Customer", EOEntity("Customer","id"))
            )
          )
        ),
        D2WContext(EOEntity("Project","id"), "query", null, null),
        showDebugButton
      )
      Future(data)
    }
  }


  def fireRuleFuture(d2WContext: D2WContext, key: String) : Future[WSResponse] = {
    val entityOpt = if (d2WContext.entity == null) None else Some(d2WContext.entity.name)
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
    //val entityName = entity.map(_.name)
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


  def propertyMetaInfosForTask(displayPropertyKeys: JsArray, entity: EOEntity, task: String) = {
    val propertiesFutures = displayPropertyKeys.value.map( x => {
      val propertyKey = x.asOpt[String].get
      val propertyDisplayNameFuture = fireRuleFuture(entity.name, task, propertyKey, "displayNameForProperty")
      val componentNameFuture = fireRuleFuture(entity.name, task, propertyKey, "componentName")
      val typeFuture = fireRuleFuture(entity.name, task, propertyKey, "attributeType")

      val subResult = for {
        pDisplayName <- propertyDisplayNameFuture
        pComponentName <- componentNameFuture
        ptype <- typeFuture
      } yield {
        val propertyDisplayName = fromResponseToString(pDisplayName)
        val propertyComponentName = fromResponseToString(pComponentName)
        val attributeType = fromResponseToString(ptype)

        println("<" + propertyComponentName + ">")

        PropertyMetaInfo(
          attributeType._2,
          D2WContext(entity, task, null, propertyKey),
          List(
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


  def fromResponseToMetaInfo(response: WSResponse, entity: EOEntity, task: String) = {
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
  def getMetaData(entityName: String): Future[EntityMetaData] = {
    if (usesD2SPAServer) {
      val entityDisplayNameFuture = fireRuleFuture(entityName, TaskDefine.edit, RuleKeys.displayNameForEntity)
      val pkAttributeNameFuture = fireRuleFuture(entityName, TaskDefine.edit, RuleKeys.pkAttributeName)
      val queryDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.query, RuleKeys.displayPropertyKeys)
      val editDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.edit, RuleKeys.displayPropertyKeys)
      val listDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.list, RuleKeys.displayPropertyKeys)
      val inspectDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.inspect, RuleKeys.displayPropertyKeys)

      val result = for {
        r1 <- entityDisplayNameFuture
        queryPkAttributeName <- pkAttributeNameFuture
        queryDisplayPropertyKeys <- queryDisplayPropertyKeysFuture
        listDisplayPropertyKeys <- listDisplayPropertyKeysFuture
        inspectDisplayPropertyKeys <- inspectDisplayPropertyKeysFuture
        editDisplayPropertyKeys <- editDisplayPropertyKeysFuture
      } yield {
        val entityDisplayName = fromResponseToString(r1)
        val pkAttributeName = fromResponseToString(queryPkAttributeName)
        val entity = EOEntity(entityName,pkAttributeName._2)

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
      if (entityName.equals("Customer")) {
        val entity = EOEntity(entityName,"id")
        Future(EntityMetaData(entity, "Customer",
          Task(
              List(
                PropertyMetaInfo(ValueType.stringV,D2WContext(entity, "query", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "query", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "query", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(ValueType.stringV,D2WContext(entity, "list", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName" ,EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "list", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(ValueType.stringV,D2WContext(entity, "inspect", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "inspect", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "inspect", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WDisplayString"))
                  ))
              )
            ) ,
            Task(
              List(
                PropertyMetaInfo(ValueType.stringV,D2WContext(entity, "edit", null, "name"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Name")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "edit", null, "acronym"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Acronym")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  )),
                PropertyMetaInfo(ValueType.stringV,
                  D2WContext(entity, "edit", null, "address"),
                  List(
                    RuleResult("displayNameForProperty",EOValueUtils.stringV("Address")),
                    RuleResult("componentName",EOValueUtils.stringV("ERD2WQueryStringOperator"))
                  ))
              )
            )
          )
        )
      } else {
        val entity = EOEntity(entityName, "id")
        Future(EntityMetaData(entity, "Project",
          Task(
            List(
              PropertyMetaInfo(ValueType.stringV, D2WContext(entity, "query", null, "descr"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Description")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WQueryStringOperator"))
                )),
              PropertyMetaInfo(ValueType.stringV,
                D2WContext(entity, "query", null, "projectNumber"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Project Number")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WQueryStringOperator"))
                ))
            )
          ),
          Task(
            List(
              PropertyMetaInfo(ValueType.stringV, D2WContext(entity, "list", null, "descr"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Description")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WQueryStringOperator"))
                ))
            )
          ),
          Task(
            List(
              PropertyMetaInfo(ValueType.stringV, D2WContext(entity, "inspect", null, "descr"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Description")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WQueryStringOperator"))
                ))
            )
          ),
          Task(
            List(
              PropertyMetaInfo(ValueType.stringV, D2WContext(entity, "edit", null, "descr"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Description")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WEditString"))
                )),
              PropertyMetaInfo(ValueType.stringV, D2WContext(entity, "edit", null, "descr"),
                List(
                  RuleResult("displayNameForProperty", EOValueUtils.stringV("Description")),
                  RuleResult("componentName", EOValueUtils.stringV("ERD2WEditString"))
                ))
            )
          )
        )
        )
      }
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


  def search(entity: EOEntity, queryValues: List[QueryValue]): Future[Seq[EO]] = {
    println("Search for entity: " + entity.name + " queryValues " + queryValues)
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



  def searchOnD2SPAServer(entity: EOEntity, queryValues: List[QueryValue]): Future[Seq[EO]] = {
    val qualifierSuffix = if (queryValues == null || queryValues.isEmpty) "" else "?" + qualifiersUrlPart(queryValues)
    val url = d2spaServerBaseUrl + "/" + entity.name + ".json" + qualifierSuffix
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
              valuesMap += (key -> EOValue(typeV = ValueType.intV, intV = Some(bigDecimal.toInt)))
            case play.api.libs.json.JsNull =>
              // TBD use a kind of Null ?
              valuesMap += (key -> EOValue())
            case _ =>
              valuesMap += (key -> EOValue(stringV = Some("not supported")))

          }
        }
        eos ::= EO(entity,valuesMap,None)
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
      case ValueType.intV => JsNumber (value.intV.get)
        // id: hardcoded
      case ValueType.eoV => {
        val eoV = value.eoV.get
        val pkAttributeName = eoV.pkAttributeName
        JsObject( Seq( pkAttributeName -> JsNumber(eoV.id), "type" -> JsString(eoV.entity)))
      }
      case _ => JsString("")
    }

  }


  // DELETE
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/2.json
  // => 2

  // Return the EO with everything and may be a validationError
  def deleteEO(eo: EO): Future[EO] = {
    val pk = EOValueUtils.pk(eo)
    pk match {
      case Some(pkValue) =>
        val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pkValue + ".json"
        println("Delete EO: " + eo)
        println("Delete WS: " + url)

        val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
        val futureResponse: Future[WSResponse] = request.delete()
        futureResponse.map { response =>
          try {
            val resultBody = response.json
            eo
          } catch {
            case parseException: JsonParseException => {
              handleException(response.body,eo)
            }
            case t: Throwable => {
              handleException(t.getMessage(),eo)
            }
          }
        }
      case _ =>
        Future(handleException("No value found for primary key in eo",eo))
    }
  }



  // POST (create)
  // call Project.json with: {"descr":"bb","projectNumber":2}
  // {"id":2,"type":"Project","descr":"bb","projectNumber":2,"customer":null}

  def newEO(entity: EOEntity, eo: EO): Future[EO] = {
    println("Update EO: " + eo)

    val url = d2spaServerBaseUrl + "/" + entity.name + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)

    // For all value the user has set, we prepare the json to be sent
    val eoDefinedValues = eo.values filter (v => {
      println("v._2" + v._2)
      EOValueUtils.isDefined(v._2) })

    println("eoDefinedValues " + eoDefinedValues)
    val eoValues = eoDefinedValues map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
    println("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    println("Upate WS:  " + url)
    println("WS post data: " + data)

    val futureResponse: Future[WSResponse] = request.post(data)
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        val jsObj = resultBody.asInstanceOf[JsObject]
        val pkAttributeName = entity.pkAttributeName
        val pkValue = jsObj.value(pkAttributeName)
        pkValue match {
          case JsNumber(pkNumber) => eo.copy(values = eo.values + (pkAttributeName -> EOValueUtils.intV(pkNumber.intValue())))
          case _ => eo
        }
      } catch {
        case parseException: JsonParseException => {
          handleException(response.body,eo)
        }
        case t: Throwable => {
          handleException(t.getMessage(),eo)
        }
      }
    }
  }

  // Put
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/2.json
  // {"descr":"bobo","projectNumber":2}
  // => returns:
  /* {
  "id": 2,
  "type": "Project",
  "descr": "bobo",
  "projectNumber": 2,
  "customer": null
}*/



  def updateEO(eo: EO): Future[EO] = {
    println("Update EO: " + eo)
    val pk = EOValueUtils.pk(eo).get

    val entity = eo.entity
    val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pk + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val eoDefinedValues = eo.values filter (v => {
      println("v._2" + v._2)
      EOValueUtils.isDefined(v._2) })

    println("eoDefinedValues " + eoDefinedValues)
    val eoValues = eoDefinedValues map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
    println("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    println("Upate WS:  " + url)
    println("WS post data: " + data)

    val futureResponse: Future[WSResponse] = request.put(data)
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        val array = resultBody.asInstanceOf[JsObject]
        eo
      } catch {
        case parseException: JsonParseException => {
          handleException(response.body,eo)
        }
        case t: Throwable => {
          handleException(t.getMessage(),eo)
        }
      }
    }
  }

  def handleException(error: String, eo: EO) : EO = {
    eo.copy(validationError = Some(error))
  }



}
