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
import play.api.libs.json.{JsString, _}
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

case class FetchedEOEntity (
                           name: String,
                           primaryKeyAttributeNames: Seq[String],
                           relationships: Seq[FetchedEORelationship] = Seq()
                           )
case class FetchedEORelationship (
                                   name: String,
                                   destinationEntityName: String)





class ApiService(config: Configuration, ws: WSClient) extends Api {
  val showDebugButton = config.getBoolean("d2spa.showDebugButton").getOrElse(true)
  val d2spaServerBaseUrl = "http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra";


  var eos = Seq(
    EO(EOEntity("Project","id",List()), Map(
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


  implicit lazy val fetchedEOEntityReads: Reads[FetchedEOEntity] = (
      (JsPath \ "name").read[String] and
      (JsPath \ "primaryKeyAttributeNames").read[Seq[String]] and
      (
        (JsPath \ "relationships").lazyRead(Reads.seq(fetchedEORelationshipReads)) or
          Reads.pure(Seq.empty[FetchedEORelationship])
        )
    )(FetchedEOEntity.apply _)

  implicit lazy val fetchedEORelationshipReads: Reads[FetchedEORelationship] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "destinationEntityName").read[String]
    )(FetchedEORelationship.apply _)

  implicit lazy val menuItemReads: Reads[MenuItem] = (
      (JsPath \ "id").read[Int] and
      (JsPath \ "type").read[String] and
      (JsPath \ "title").read[String] and
      ((JsPath \ "entity").read[EntityItem] orElse(Reads.pure(null))) and
      (JsPath \ "parent").lazyReadNullable(menuItemReads)
    )(MenuItem.apply _)

// case class EORef(entity: String, displayName: String, pk: Int)

  implicit val eoRefReads: Reads[EORef] = (
      (JsPath \ "entityName").read[String] and
      (JsPath \ "id").read[Int]
    ) (EORef.apply _)


  var _eomodelF: Option[Future[EOModel]] = None
  var _eomodel: Option[EOModel] = None

  def eomodel(): EOModel = {
    if (!_eomodel.isDefined) {
      println("fetch eomodel")
      _eomodel = Some(Await.result(eomodelF(),10 seconds))
    }
    _eomodel.get
  }


  def eomodelF(): Future[EOModel] = {
    if (!_eomodelF.isDefined) {
      println("fetch eomodel future")
      _eomodelF = Some(executeEOModelWS())
    }
    _eomodelF.get
  }


  def fetchEOModel() : Future[EOModel] = {
    eomodelF()
  }


  // case class EOEntity(name: String, pkAttributeName: String, relationships: List[EORelationship])
  // case class EORelationship(name: String, destinationEntityName: String)

  def executeEOModelWS() : Future[EOModel] = {
    val url = d2spaServerBaseUrl + "/EOModel.json";
    println("WS " + url)
    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      println("Eomodels " + resultBody)
      var entities = List[EOEntity]()

      val modelArray = resultBody.asInstanceOf[JsArray].value
      for (model <- modelArray) {
        val eomodelJsObj = model.asInstanceOf[JsObject]
        val array = (eomodelJsObj \ "entities").get.asInstanceOf[JsArray].value
        println("Entities " + array)

        for (menuRaw <- array) {
          //println(menuRaw)
          val obj = menuRaw.validate[FetchedEOEntity]
          obj match {
            case s: JsSuccess[FetchedEOEntity] => {
              val fetchedEOEntity = s.get
              val primaryKeyAttributeName = fetchedEOEntity.primaryKeyAttributeNames(0)
              val fetchedRelationships = fetchedEOEntity.relationships
              val relationships = fetchedRelationships.map(r => EORelationship(r.name,r.destinationEntityName)).toList
              entities =  EOEntity(fetchedEOEntity.name, primaryKeyAttributeName, relationships) :: entities
            }
            case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString())
          }
        }
      }
      println("Entities " + entities)
      EOModel(entities)
    }
  }


  override def getMenus(): Future[Menus] = {
    println("get Menus")

      val fetchedEOModel = eomodel()
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

            val childMenus = cs.map(cm => {
              println("LOOK for " + cm.entity.name + " into eomodel " + fetchedEOModel)

              val entity = EOModelUtils.entityNamed(fetchedEOModel,cm.entity.name).get
              Menu(cm.id,cm.title,entity)
            })
            MainMenu(mm.id,mm.title,childMenus)
        }
        if (mainMenus.isEmpty) {
          // TOTO Menus containing D2WContext is not a good choice because better to have
          // None D2WContext if no menus (at least, D2WContext should be an option instead of returning
          // D2WContext(null,null,null)

          Menus(List(),D2WContext(null,null,null,null),showDebugButton)
        } else {
          val firstChildEntity = mainMenus.head.children.head.entity
          Menus(mainMenus.toList,D2WContext(firstChildEntity.name,"query",null,null),showDebugButton)
        }
      }


  }


  def fireRuleFuture(d2WContext: D2WContext, key: String) : Future[WSResponse] = {
    val entityName = d2WContext.entityName
    val taskOpt = if (d2WContext.task == null) None else Some(d2WContext.task)
    fireRuleFuture(Some(entityName), taskOpt, d2WContext.propertyKey, key)
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
          propertyKey,
          entity.name,
          task,
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
    val fetchedEOModel = eomodel()
      val entityDisplayNameFuture = fireRuleFuture(entityName, TaskDefine.edit, RuleKeys.displayNameForEntity)
      val queryDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.query, RuleKeys.displayPropertyKeys)
      val editDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.edit, RuleKeys.displayPropertyKeys)
      val listDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.list, RuleKeys.displayPropertyKeys)
      val inspectDisplayPropertyKeysFuture = fireRuleFuture(entityName, TaskDefine.inspect, RuleKeys.displayPropertyKeys)

      val result = for {
        r1 <- entityDisplayNameFuture
        queryDisplayPropertyKeys <- queryDisplayPropertyKeysFuture
        listDisplayPropertyKeys <- listDisplayPropertyKeysFuture
        inspectDisplayPropertyKeys <- inspectDisplayPropertyKeysFuture
        editDisplayPropertyKeys <- editDisplayPropertyKeysFuture
      } yield {
        val entityDisplayName = fromResponseToString(r1)
        println("LOOK for " + entityName + " into eomodel " + fetchedEOModel)
        val entity = EOModelUtils.entityNamed(fetchedEOModel,entityName).get

        val queryProperties = fromResponseToMetaInfo(queryDisplayPropertyKeys,entity,TaskDefine.query)
        val listProperties = fromResponseToMetaInfo(listDisplayPropertyKeys,entity,TaskDefine.list)
        val inspectProperties = fromResponseToMetaInfo(inspectDisplayPropertyKeys,entity,TaskDefine.inspect)
        val editProperties = fromResponseToMetaInfo(editDisplayPropertyKeys,entity,TaskDefine.edit)

        val emd = EntityMetaData(entity, entityDisplayName._2, Task(queryProperties), Task(listProperties),Task(inspectProperties),Task(editProperties))
        emd
      }
      result



  }

  // API method
  def fireRules(rules: Map[String,D2WContext]): Future[List[RuleResult]] = {
    val futures = rules.map{ case (key, d2wContext) => {fireRuleFuture(d2wContext, key)}}
    val futureSequece = Future sequence futures
    val result = for {
      f <- futureSequece
    } yield {
      f.map(ruleResultWithResponse(_)).toList
    }
    result
  }


  def ruleResultWithResponse(rule: FireRule, response: WSResponse) = {
    val jsObj = response.json.asInstanceOf[JsObject]
    val key = jsObj.keys.toSeq(0)

    println("Rule response: " + jsObj)

    val value = if (key.equals(RuleKeys.destinationEos)) {
      // http://localhost:1666//cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?entity=Project&task=edit&propertyKey=customer&key=destinationEos
      /* Response:
      {
        "destinationEos": [
          {
            "id": 1,
            "entity": "Customer"
          }
        ]
      }
       */
      val rules = (jsObj \ RuleKeys.destinationEos).get.asInstanceOf[JsArray].value
      println("Rule response value: " + rules.getClass.getName)
      val eoRefs = rules.map (x => {
        //println("x: " + x + " x " + x.getClass.getName)
        val jsObj = x.asInstanceOf[JsObject]
        val entityName = jsObj.value("entity").asInstanceOf[JsString].value
        val pkAttributeName = pkAttributeNameForEntityNamed(entityName)
        val id = jsObj.value(pkAttributeName).asInstanceOf[JsNumber].value.intValue()

        EORef(entityName,id)
      })
      // val eoRefs = eoRefJsSuccesses.filter(x => x.isSuccess).map(_.get)
      println("EoRefs " + eoRefs)
      EOValueUtils.eosV(eoRefs.toSeq)
    } else {
      // http://localhost:1666//cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?entity=Project&task=edit&propertyKey=customer&key=keyWhenRelationship
      /* Response:
      {
         "keyWhenRelationship": "name"
      }
       */
      EOValueUtils.stringV(jsObj.values.toSeq(0).asOpt[String].get)
    }
    RuleResult(rule.rhs, rule.key, value)
  }

  def fireRule(rule: FireRule): Future[RuleResult] = {
    val f = fireRuleFuture(rule.rhs, rule.key)
    f.map(ruleResultWithResponse(rule,_))
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
            case play.api.libs.json.JsObject(fields) =>
              // case class EORef(entityName: String, id: Int)
              val fieldsMap = fields.toMap
              println("fieldsMap " + fieldsMap)
              val destinationEntityName = fieldsMap("type").asInstanceOf[JsString].value
              val destinationEntity = EOModelUtils.entityNamed(eomodel(),destinationEntityName).get
              println("destinationEntity " + destinationEntity)
              val pkName = destinationEntity.pkAttributeName
              val pk = fieldsMap(pkName).asInstanceOf[JsNumber].value.toInt
              valuesMap += (key -> EOValue(typeV = ValueType.eoV, eoV = Some(EORef(destinationEntityName,pk))))
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

  /* stays with future

  def pkAttributeNameForEntityNamed(entityName: String): String = {
    eomodel.onComplete({
      case Success(model) => {
        val entityOpt = model.entities.find(e => e.name.equals(entityName))
        entityOpt match {
          case Some(entity) => entity.pkAttributeName
          case None => ""
        }

      }
      case Failure(exception) => {
        ""
      }
    })
  }*/

  def pkAttributeNameForEntityNamed(entityName: String): String = {
    val entityOpt = eomodel().entities.find(e => e.name.equals(entityName))
    entityOpt match {
      case Some(entity) => entity.pkAttributeName
      case None => ""
    }
  }

  def woWsParameterForValue(value: EOValue): JsValue = {
    value.typeV match {
      case ValueType.stringV =>
        value.stringV match {
          case Some(stringValue) =>
            JsString(stringValue)
          case _ =>
            JsNull
        }
      case ValueType.intV =>
        value.intV match {
          case Some(intValue) =>
            JsNumber(intValue)
          case _ =>
            JsNull
        }
      case ValueType.eoV => {
        value.eoV match {
          case Some(eoRef) =>
            val eoRef = value.eoV.get
            val pkAttributeName = pkAttributeNameForEntityNamed(eoRef.entityName)
            JsObject( Seq( pkAttributeName -> JsNumber(eoRef.id), "type" -> JsString(eoRef.entityName)))
          case _ => JsNull
        }
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

  // Get
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

  def completeEO(eo: EO, missingKeys: Set[String]): Future[EO] = {
    println("Complete EO: " + eo)
    val pk = EOValueUtils.pk(eo).get

    val entity = eo.entity
    // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Customer/2/propertyValues.json?missingKeys=("projects")
    val toArrayString= missingKeys.map(x => s""""${x}"""").mkString("(",",",")")
    val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pk + "/propertyValues.json?missingKeys=" + toArrayString


    // returns
    // {
    //  "id": 2,
    //  "type": "Customer",
    //  "projects": [
    //    {
    //      "id": 1,
    //      "type": "Project"
    //    }
    //  ]
    // }
    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)

    val futureResponse: Future[WSResponse] = request.get
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        val jObj = resultBody.asInstanceOf[JsObject]

        val pkName = eo.entity.pkAttributeName
        // Seq[(String, JsValue)]
        val jObjValues = jObj.fields.filter(x => !(x._1.equals(pkName) || x._1.equals("type")))
//case class EORef(entity: String, displayName: String, id: Int, pkAttributeName: String)
// case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])

        val newValues = jObjValues.map ( kvTuple => {
          val value = kvTuple._2
          // JsValue : JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsUndefined
          val newValue = if (value.isInstanceOf[play.api.libs.json.JsArray]) {
            val seqJsValues = value.asInstanceOf[play.api.libs.json.JsArray].value
            // {"id":1,"type":"Project"}
            val eoRefs = seqJsValues.map (x => {
              val jsEO = x.asInstanceOf[JsObject].fields.toList.toMap
              val entityName = jsEO("type").asInstanceOf[JsString].value
              val id = jsEO("id").asInstanceOf[JsNumber].value.toInt

              // !!! Hardcoding of displayName and "id" as if always "id" for primary Key name
              EORef(entityName,id)
            })
            EOValueUtils.eosV(eoRefs)
          } else if (value.isInstanceOf[JsString]) {
            val stringV = value.asInstanceOf[play.api.libs.json.JsString].value
            EOValueUtils.stringV(stringV)
          } else {
            EOValueUtils.stringV(value.toString())
          }
          println("JsObj value " + value.getClass.getName + " value: " + value)
          (kvTuple._1,newValue)
        }).toMap

        val updatedValues = eo.values ++ newValues
        eo.copy(values = updatedValues)
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

// Upate WS:  http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/3.json
//  WS post data: {"descr":"1","id":3,"customer":{"id":2,"type":"Customer"},"projectNumber":3,"type":"Project"}

  def updateEO(eo: EO): Future[EO] = {
    println("Update EO: " + eo)
    val pk = EOValueUtils.pk(eo).get

    val entity = eo.entity
    val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pk + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    /*val eoDefinedValues = eo.values filter (v => {
      println("v._2" + v._2)
      EOValueUtils.isDefined(v._2) })*/

    println("eoDefinedValues " + eo.values)
    val eoValues = eo.values map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
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
