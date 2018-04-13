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



  var _eomodelF: Option[Future[EOModel]] = None
  var _eomodel: Option[EOModel] = None

  def eomodel(): EOModel = {
    if (!_eomodel.isDefined) {
      Logger.debug("fetch eomodel")
      _eomodel = Some(Await.result(eomodelF(),10 seconds))
    }
    _eomodel.get
  }


  def eomodelF(): Future[EOModel] = {
    if (!_eomodelF.isDefined) {
      Logger.debug("fetch eomodel future")
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
    Logger.debug("WS " + url)
    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      Logger.debug("Eomodels " + resultBody)
      var entities = List[EOEntity]()

      val modelArray = resultBody.asInstanceOf[JsArray].value
      for (model <- modelArray) {
        val eomodelJsObj = model.asInstanceOf[JsObject]
        val array = (eomodelJsObj \ "entities").get.asInstanceOf[JsArray].value
        Logger.debug("Entities " + array)

        for (menuRaw <- array) {
          //Logger.debug(menuRaw)
          val obj = menuRaw.validate[FetchedEOEntity]
          obj match {
            case s: JsSuccess[FetchedEOEntity] => {
              val fetchedEOEntity = s.get
              val primaryKeyAttributeName = fetchedEOEntity.primaryKeyAttributeNames(0)
              val fetchedRelationships = fetchedEOEntity.relationships
              val relationships = fetchedRelationships.map(r => EORelationship(r.name,r.destinationEntityName)).toList
              entities =  EOEntity(fetchedEOEntity.name, primaryKeyAttributeName, relationships) :: entities
            }
            case e: JsError => Logger.error("Errors: " + JsError.toFlatJson(e).toString())
          }
        }
      }
      Logger.debug("Entities " + entities)
      EOModel(entities)
    }
  }


  override def getMenus(): Future[Menus] = {
    Logger.debug("get Menus")

      val fetchedEOModel = eomodel()
      val url = d2spaServerBaseUrl + "/Menu.json";
      val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
      val futureResponse: Future[WSResponse] = request.get()
      futureResponse.map { response =>
        val resultBody = response.json
        val array = resultBody.asInstanceOf[JsArray]
        var menus = List[MenuItem]()
        for (menuRaw <- array.value) {
          //Logger.debug(menuRaw)
          val obj = menuRaw.validate[MenuItem]
          obj match {
            case s: JsSuccess[MenuItem] => {
              val wiObj = s.get
              menus = wiObj :: menus
            }
            case e: JsError => Logger.error("Errors: " + JsError.toFlatJson(e).toString())
          }
        }
        val children = menus.filter(_.parent.isDefined)
        val childrenByParent = children.groupBy(_.parent.get)

        val mainMenus = childrenByParent.map{
          case (mm, cs) =>

            val childMenus = cs.map(cm => {
              Logger.debug("LOOK for " + cm.entity.name + " into eomodel " + fetchedEOModel)

              val entity = EOModelUtils.entityNamed(fetchedEOModel,cm.entity.name).get
              Menu(cm.id,cm.title,entity)
            })
            MainMenu(mm.id,mm.title,childMenus)
        }
        if (mainMenus.isEmpty) {
          // TOTO Menus containing D2WContext is not a good choice because better to have
          // None D2WContext if no menus (at least, D2WContext should be an option instead of returning
          // D2WContext(null,null,null)

          Menus(List(),showDebugButton)
        } else {
          val firstChildEntity = mainMenus.head.children.head.entity
          Menus(mainMenus.toList,showDebugButton)
        }
      }
  }

  val fireRuleArguments = List("entity","task","propertyKey","pageConfiguration","key")

  def fireRuleFuture(rhs: D2WContextFullFledged, key: String) : Future[WSResponse] = {
    //Logger.debug("Fire Rule for key " + key + " rhs:" + rhs)
    val url = d2spaServerBaseUrl + "/fireRuleForKey.json";
    //val entityName = entity.map(_.name)
    val fireRuleValues = List(rhs.entityName, rhs.task, rhs.propertyKey, rhs.pageConfiguration, Some(key))
    val nonNullArguments = fireRuleArguments zip fireRuleValues
    val arguments = nonNullArguments.filter(x => !x._2.isEmpty).map(x => (x._1, x._2.get))

    //Logger.debug("Args : " + arguments)

    val request: WSRequest = ws.url(url)
      .withQueryString(arguments.toArray: _*)
      .withRequestTimeout(10000.millis)
    request.get()
  }

  private def lift[T](futures: Seq[Future[T]]) =
    futures.map(_.map { Success(_) }.recover { case t => Failure(t) })


  def waitAll[T](futures: Seq[Future[T]]) =
    Future.sequence(lift(futures)) // having neutralized exception completions through the lifting, .sequence can now be used


  // Fetch the following information for each property
  // - displayNameForProperty
  // - componentName
  // - attributeType
  def propertyMetaInfosForTask(displayPropertyKeys: Seq[String], entity: EOEntity, task: String) = {
    val propertiesFutures = displayPropertyKeys.map( propertyKey => {
      val rhs = D2WContextFullFledged(Some(entity.name),Some(task),Some(propertyKey))
      val propertyDisplayNameFuture = fireRuleFuture(rhs, "displayNameForProperty")
      val componentNameFuture = fireRuleFuture(rhs, "componentName")
      val typeFuture = fireRuleFuture(rhs, "attributeType")

      val subResult = for {
        pDisplayName <- propertyDisplayNameFuture
        pComponentName <- componentNameFuture
        ptype <- typeFuture
      } yield {
        val propertyDisplayName = fromRuleResponseToKeyAndString(pDisplayName)
        val propertyComponentName = fromRuleResponseToKeyAndString(pComponentName)
        val attributeType = fromRuleResponseToKeyAndString(ptype)

        //Logger.debug("<" + propertyComponentName + ">")

        PropertyMetaInfo(
          attributeType._2,
          propertyKey,
          entity.name,
          task,
          List(
            RuleResult(rhs, propertyDisplayName._1, RuleValue(Some(propertyDisplayName._2))),
            RuleResult(rhs, propertyComponentName._1, RuleValue(Some(propertyComponentName._2)))
            //RuleResult(RuleUtils.convertD2WContextToFullFledged(rhs), propertyDisplayName._1, propertyDisplayName._2),
            //RuleResult(RuleUtils.convertD2WContextToFullFledged(rhs), propertyComponentName._1, propertyComponentName._2)
          )
        )
      }
      subResult
    }).toList
    val futureOfList = Future sequence propertiesFutures
    val properties = Await result (futureOfList, 2 seconds)
    properties
  }

  def fromRuleResponseToKeyAndString(response: WSResponse) = {
    val jsObj = response.json.asInstanceOf[JsObject]
    val key = jsObj.keys.toSeq(0)
    val valueOpt = jsObj.values.toSeq(0).asOpt[String]
    val value = valueOpt match {
      case Some(astring) => astring
      case _ => ""
    }
    (key, value)
  }
  def fromRuleResponseToKeyAndArray(response: WSResponse) = {
    val jsObj = response.json.asInstanceOf[JsObject]
    val key = jsObj.keys.toSeq(0)
    val valueOpt = jsObj.values.toSeq(0).asOpt[JsArray]
    val value = valueOpt match {
      case Some(jsarray) => jsarray.value.map(x => x.asOpt[String].get)
      case _ => Seq.empty[String]
    }
    (key, value)
  }


  def fromResponseToMetaInfo(response: WSResponse, entity: EOEntity, task: String) = {
    val displayPropertyKeys = fromRuleResponseToKeyAndArray(response)
    //Logger.debug("--- " + task + " --- " + array)
    propertyMetaInfosForTask(displayPropertyKeys._2, entity, task)
  }

  // To create the result, it needs to issue multiple query on the D2W rule system
  // 1) get entity display name for edit, inspect, query, list
  // D2WContext: task=edit, entity=..
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayNameForEntity
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayPropertyKeys
  def getMetaData(d2wContext: D2WContextFullFledged): Future[EntityMetaData] = {
    val fetchedEOModel = eomodel()
      val entityDisplayNameFuture = fireRuleFuture(d2wContext, RuleKeys.displayNameForEntity)
      val displayPropertyKeysFuture = fireRuleFuture(d2wContext, RuleKeys.displayPropertyKeys)

      val result = for {
        r1 <- entityDisplayNameFuture
        displayPropertyKeys <- displayPropertyKeysFuture
      } yield {
        val entityDisplayName = fromRuleResponseToKeyAndString(r1)
        val entityName = d2wContext.entityName.get
        Logger.debug("LOOK for " + entityName + " into eomodel " + fetchedEOModel)
        val entity = EOModelUtils.entityNamed(fetchedEOModel,entityName).get
        val task = d2wContext.task.get

        val properties = fromResponseToMetaInfo(displayPropertyKeys,entity,task)

        val emd = EntityMetaData(d2wContext, entityDisplayName._2, properties)
        emd
      }
      result



  }



  def fireRule(rhs: D2WContextFullFledged, key: String): Future[RuleResult] = {
    Logger.debug("Fire rule for key " + key + " and d2wContext: " + rhs)
    val f = fireRuleFuture(rhs, key)
    f.map(ruleResultWithResponse(rhs,_))
  }


  def ruleResultWithResponse(rhs: D2WContextFullFledged, response: WSResponse) = {
    val jsObj = response.json.asInstanceOf[JsObject]
    val key = jsObj.keys.toSeq(0)

    Logger.debug("Rule response: " + jsObj)

      // http://localhost:1666//cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?entity=Project&task=edit&propertyKey=customer&key=keyWhenRelationship
      /* Response:
      {
         "keyWhenRelationship": "name"
      }
       */
    val jsvalue = jsObj.values.toSeq(0)
    Logger.debug("jsvalue " + jsvalue)
    val ruleValue = if (jsvalue.asOpt[JsArray].isDefined) {
      val (key, value) = fromRuleResponseToKeyAndArray(response)
      RuleValue(stringsV = value.toList)
    } else {
      val (key, value) = fromRuleResponseToKeyAndString(response)
      Logger.debug("key  " + key + " value "+ value)
      RuleValue(Some(value))
    }

    val result  =RuleResult(rhs, key, ruleValue)
    Logger.debug("Result " + result )
    result
    //RuleResult(RuleUtils.convertD2WContextToFullFledged(rhs), key, ruleValue.stringV.get)
  }

  def hydrateEOs(eo: Seq[EO],missingKeys: Set[String]): scala.concurrent.Future[Seq[d2spa.shared.EO]] = {
    Future(Seq())
  }

  def searchAll(fs: EOFetchAll): Future[Seq[EO]] = {
    val entityName = EOFetchSpecification.entityName(fs)

    searchOnD2SPAServer(entityName,None)
  }
  def search(fs: EOQualifiedFetch): Future[Seq[EO]] = {
    val entityName = EOFetchSpecification.entityName(fs)

    searchOnD2SPAServer(entityName,Some(fs.qualifier))
  }



  // TBD Sorting parameter
  // qualifier=product.name='CMS' and parentProductReleases.customer.acronym='ECHO'&sort=composedName|desc
  def qualifiersUrlPart(q: EOQualifier) : String = {
    val qualifiersStrings = q match {
      case EOAndQualifier(qualifiers) => qualifiers.map(q => qualifierUrlPart(q.asInstanceOf[EOKeyValueQualifier]))
      case _ => List()
    }
    return qualifiersStrings.mkString(" and ")
  }

  def qualifierUrlPart(qualifier: EOKeyValueQualifier) : String = {
    val value = qualifier.value
    return value match {
      case StringValue(stringV) => "qualifier=" + qualifier.key + " like '*" + stringV.get + "*'"
      case IntValue(i)  => "" // TODO
      case ObjectValue(isSome, eo) => "" // TODO
      // To Restore case ObjectsValue(eos) => "" // TODO
    }
  }



  def searchOnD2SPAServer(entityName: String, qualifierOpt: Option[EOQualifier]): Future[Seq[EO]] = {
    Logger.debug("Search with fs:" + entityName)
    val entity = EOModelUtils.entityNamed(eomodel(),entityName).get


    val qualifierSuffix = qualifierOpt match {
      case Some(q) => "?" + qualifiersUrlPart(q)
      case _ => ""
    }
    val url = d2spaServerBaseUrl + "/" + entityName + ".json" + qualifierSuffix
    Logger.debug("Search URL:" + url)
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
          //Logger.debug("value class " + value.getClass.getName)
          value match {
            case s: play.api.libs.json.JsString =>
              valuesMap += (key -> StringValue(Some(s.value)))
            case n: play.api.libs.json.JsNumber =>
              val bigDecimal = n.value
              // TBD use a BigDecimal container
              valuesMap += (key -> IntValue(Some(bigDecimal.toInt)))
            case play.api.libs.json.JsNull =>
              // TBD use a kind of Null ?
              valuesMap += (key -> StringValue(None))
            case play.api.libs.json.JsObject(fields) =>
              // case class EORef(entityName: String, id: Int)
              val fieldsMap = fields.toMap
              Logger.debug("fieldsMap " + fieldsMap)
              val destinationEntityName = fieldsMap("type").asInstanceOf[JsString].value
              val destinationEntity = EOModelUtils.entityNamed(eomodel(),destinationEntityName).get
              Logger.debug("destinationEntity " + destinationEntity)
              val pkName = destinationEntity.pkAttributeName
              val pk = fieldsMap(pkName).asInstanceOf[JsNumber].value.toInt
              val eo = EOValue.dryEOWithEntity(destinationEntity,Some(pk))
              valuesMap += (key -> ObjectValue(eo = eo))
            case _ =>
              valuesMap += (key -> StringValue(Some("not supported")))

          }
        }
        val eo = EO(entity,valuesMap,0)
        val pk = EOValue.pk(eo).get
        val updatedEO = eo.copy(pk = pk)
        eos ::= updatedEO
      }
      Logger.debug("Search: eos created " + eos)

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
          case e: JsError => Logger.error("Errors: " + JsError.toFlatJson(e).toString())
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
    value match {
      case StringValue(stringV) =>
        stringV match {
          case Some(stringValue) =>
            JsString(stringValue)
          case _ =>
            JsNull
        }
      case IntValue(intV) =>
        intV match {
          case Some(intValue) =>
            JsNumber(intValue)
          case _ =>
            JsNull
        }
      case ObjectValue(isSome, eo) => {
        if (isSome) {

          val entityName = eo.entity.name
          val pkAttributeName = pkAttributeNameForEntityNamed(entityName)
          val pk = EOValue.pk(eo).get
          JsObject(Seq(pkAttributeName -> JsNumber(pk), "type" -> JsString(entityName)))
        } else {
          JsNull
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
    val pk = EOValue.pk(eo)
    pk match {
      case Some(pkValue) =>
        val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pkValue + ".json"
        Logger.debug("Delete EO: " + eo)
        Logger.debug("Delete WS: " + url)

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

  def newEO(entityName: String, eo: EO): Future[EO] = {
    Logger.debug("New EO: " + eo)

    val url = d2spaServerBaseUrl + "/" + entityName + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)

    // For all value the user has set, we prepare the json to be sent
    val eoDefinedValues = eo.values filter (v => {
      Logger.debug("v._2" + v._2)
      EOValue.isDefined(v._2) })

    Logger.debug("eoDefinedValues " + eoDefinedValues)
    val eoValues = eoDefinedValues map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
    Logger.debug("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    Logger.debug("Upate WS:  " + url)
    Logger.debug("WS post data: " + data)

    val futureResponse: Future[WSResponse] = request.post(data)
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        val jsObj = resultBody.asInstanceOf[JsObject]
        val entity = EOModelUtils.entityNamed(eomodel(),entityName).get

        val pkAttributeName = entity.pkAttributeName
        val pkValue = jsObj.value(pkAttributeName)
        pkValue match {
          case JsNumber(pkNumber) => eo.copy(values = eo.values + (pkAttributeName -> EOValue.intV(pkNumber.intValue())))
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

  def completeEO(eoFault: EOFault, missingKeys: Set[String]): Future[EO] = {
    Logger.debug("Complete EO: " + eoFault)
    // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Customer/2/propertyValues.json?missingKeys=("projects")
    val toArrayString= missingKeys.map(x => s""""${x}"""").mkString("(",",",")")
    val url = d2spaServerBaseUrl + "/" + eoFault.entityName + "/" + eoFault.pk + "/propertyValues.json?missingKeys=" + toArrayString
    val entity = EOModelUtils.entityNamed(eomodel(), eoFault.entityName).get
    val eo = EOValue.dryEOWithEntity(entity,Some(eoFault.pk))


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

        val pkName = entity.pkAttributeName
        // Seq[(String, JsValue)]
        val jObjValues = jObj.fields.filter(x => !(x._1.equals(pkName) || x._1.equals("type")))
//case class EORef(entity: String, displayName: String, id: Int, pkAttributeName: String)
// case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])

        val newValues = jObjValues.map ( kvTuple => {
          val value = kvTuple._2

          Logger.debug("Complete EO: value : " + value)
          // JsValue : JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsUndefined
          val newValue = value match {
            case jsArray: play.api.libs.json.JsArray =>
              val seqJsValues = jsArray.value
              // {"id":1,"type":"Project"}
              val eos = seqJsValues.map (x => {
                eoWithJObj(x.asInstanceOf[JsObject])
              })
              val eoPks = eos.map(eo => eo.pk)
              ObjectsValue(eoPks)

            case jsObj: JsObject =>
              ObjectValue(eo = eoWithJObj(jsObj))

            case jsString: JsString =>
              val stringV = jsString.value
              EOValue.stringV(stringV)

            case _ =>
              val stringV = value.toString()
              EOValue.stringV(stringV)

          }

          Logger.debug("JsObj value " + value.getClass.getName + " value: " + value)
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


  def eoWithJObj (jsObj: JsObject) = {
    val jsEO = jsObj.fields.toList.toMap
    val entityName = jsEO("type").asInstanceOf[JsString].value
    val id = jsEO("id").asInstanceOf[JsNumber].value.toInt

    // !!! Hardcoding of displayName and "id" as if always "id" for primary Key name
    EOValue.dryEOWith(eomodel(),entityName,Some(id))
  }

// Upate WS:  http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/3.json
//  WS post data: {"descr":"1","id":3,"customer":{"id":2,"type":"Customer"},"projectNumber":3,"type":"Project"}

  def updateEO(eo: EO): Future[EO] = {
    Logger.debug("Update EO: " + eo)
    val pk = EOValue.pk(eo).get

    val entity = eo.entity
    val url = d2spaServerBaseUrl + "/" + eo.entity.name + "/" + pk + ".json"

    val request: WSRequest = WS.url(url).withRequestTimeout(10000.millis)
    /*val eoDefinedValues = eo.values filter (v => {
      Logger.debug("v._2" + v._2)
      EOValueUtils.isDefined(v._2) })*/

    Logger.debug("eoDefinedValues " + eo.values)
    val eoValues = eo.values map {case (key,valContainer) => (key, woWsParameterForValue(valContainer))}
    Logger.debug("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    Logger.debug("Upate WS:  " + url)
    Logger.debug("WS post data: " + data)

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
