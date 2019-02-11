package models

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.event.LoggingReceive
import com.fasterxml.jackson.core.JsonParseException
import com.google.inject.assistedinject.Assisted
import com.typesafe.config.ConfigFactory
import d2spa.shared._
import javax.inject.Inject
import models.EOModelActor.{EOModelResponse, GetEOModel}
import models.EORepoActor._
import models.RulesActor.{GetRule, RuleResultsResponse}
import play.api.{Configuration, Logger}
import play.api.Play.current
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.ws._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}



object EORepoActor {

  def props(eomodelActor: ActorRef, ws: WSClient): Props = Props(new EORepoActor(eomodelActor, ws))


  //case class SearchAll(fs: EOFetchAll, requester: ActorRef) //: Future[Seq[EO]]
  case class HydrateAll(fs: EOFetchAll, requester: ActorRef) //: Future[Seq[EO]]


  //case class Search(fs: EOFetchSpecification, requester: ActorRef) //: Future[Seq[EO]]

  //case class CompleteEO(d2wContext: D2WContext, eo: EOFault, missingKeys: Set[String], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]
  case class HydrateEOs(entityName: String, pks: Seq[EOPk], missingKeys: Set[String], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[Seq[EO]]
  case class Hydrate(d2wContextOpt: Option[D2WContext], hydration: Hydration, ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[Seq[EO]]

  case class NewEO(d2wContext: D2WContext, eo: EO, ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]
  case class UpdateEO(d2wContext: D2WContext, eos: List[EO], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]

  case class DeleteEO(eo: EO, requester: ActorRef) // : Future[EO]

  // Responses
  case class FetchedObjects(entityName: String, eos: List[EO], ruleResults: Option[List[RuleResult]])
  case class CompletedEOs(d2wContext: Option[D2WContext], hydration: Hydration, eo: List[EO], ruleResults: Option[List[RuleResult]])
  //case class FetchedObjectsForList(fs: EOFetchSpecification, eos: List[EO])

  case class SavingResponse(d2wContext: D2WContext, eos: List[EO], ruleResults: Option[List[RuleResult]])
  case class DeletingResponse(eo: EO)

  def valueMapWithJsonFields(eomodel: EOModel, fields : Seq[(String,JsValue)]): Map[String,EOValue] = {
    //println("valueMapWithJsonFields | eomodel " + eomodel)
    //println("valueMapWithJsonFields | fields " + fields)

    fields.map(kvTuple => {
      val key = kvTuple._1
      val value = kvTuple._2
      //Logger.debug("valueMapWithJsonFields : " + value)
      //println("valueMapWithJsonFields : " + value)

      //Logger.debug("Complete EO: value : " + value)
      // JsValue : JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsUndefined
      val newValue = value match {
        case jsArray: play.api.libs.json.JsArray =>
          println("valueMapWithJsonFields | value | match with JsArray " + jsArray)
          println("jsArray " + jsArray.getClass.getName)


          val seqJsValues = jsArray.value
          Logger.debug("seqJsValues : " + seqJsValues)

          if (seqJsValues.size == 0) {
            EmptyValue
          } else {
            seqJsValues.head match {
              case oneElement: JsObject =>
                // {"id":1,"type":"Project"}

                val eoOpts: List[Option[EO]] = seqJsValues.toList.map(x => {
                  eoFromJsonJsObject(eomodel, None, x.asInstanceOf[JsObject])
                })
                /*val hasError = !eos.find(eoOpt => eoOpt.isEmpty).isEmpty
                if (hasError) {
                  handleException(response.body, eo)

                }*/
                println("related eos : " + eoOpts)


                val eos = eoOpts.flatten
                val eoPks = eos.map(eo => eo.pk)
                ObjectsValue(eoPks)

              case oneElement: JsNumber =>
                Logger.error("Key: " + key + " has a value of JsArray of JsNumber: " + seqJsValues)

                // {"id":1,"type":"Project"}
                /*val ints: List[Option[Int]] = seqJsValues.toList.map(n => {
                  n match {
                    case JsNumber(num) =>
                      num match {
                        case bd: BigDecimal => Some(bd.toInt)
                        case _ => {
                          Logger.error("Found Unsupported JsNumber type " + n)
                          None
                        }
                      }
                    case _ =>
                      Logger.error("Non herogeneous array. Expected JsNumber. Found: " + n)
                      None
                  }

                })
                /*val hasError = !eos.find(eoOpt => eoOpt.isEmpty).isEmpty
                if (hasError) {
                  handleException(response.body, eo)

                }*/
                val pks: List[Int] = ints.flatten
                val eo = EOValue.dryEOWithEntity(entity, pks)
                ObjectValue*/
                EmptyValue
              case _ =>
                Logger.error("Key: " + key + " has a value of JsArray of unsupported element: " + seqJsValues.head)
                EmptyValue
            }

          }



        case jsObj : JsObject =>
          println("valueMapWithJsonFields | value | match with JsObject " + jsObj)
          val eoOpt = eoFromJsonJsObject(eomodel, None, jsObj)
          eoOpt match {
            case Some(eo) =>
              ObjectValue(eo = eo.pk)
            case None =>
              println("Failed to create EO with json " + jsObj)
              EmptyValue

          }

        case jsString: JsString =>
          println("valueMapWithJsonFields | value | match with JsString " + jsString)
          val stringV = jsString.value
          EOValue.stringV(stringV)

        case play.api.libs.json.JsNumber(n) =>
          println("valueMapWithJsonFields | value | match with JsNumber " + n)
          // TBD use a BigDecimal container
          n match {
            case bd: BigDecimal => IntValue(bd.toInt)
            case _ => {
              Logger.error("Found Unsupported JsNumber type " + n)
              EmptyValue
            }
          }

        case n: play.api.libs.json.JsBoolean =>
          println("valueMapWithJsonFields | value | match with JsBoolean " + n)
          val boolVal = n.value
          BooleanValue(boolVal)

        case play.api.libs.json.JsNull =>
          println("valueMapWithJsonFields | value | match with JsNull")
          EmptyValue

        case _ =>
          println("valueMapWithJsonFields | value | match with unsupported " + _)
          EmptyValue
          //val stringV = value.toString()
          //EOValue.stringV(stringV)

      }

      //Logger.debug("JsObj value " + value.getClass.getName + " value: " + value)
      (key, newValue)
    }).toMap

  }


  def eoFromJsonJsObject(eomodel: EOModel, eoOpt: Option[EO], jsObj: JsObject): Option[EO] = {
    println("eoFromJsonJsObject | eoOpt " + eoOpt)
    println("eoFromJsonJsObject | jsObj " + jsObj)
    val entityNameOpt : Option[(String, JsValue)] = jsObj.fields.find(x => x._1.equals("type"))
    println("eoFromJsonJsObject | entityNameOpt " + entityNameOpt)
    entityNameOpt match {
      case Some((_, JsString(entityName))) =>
        val entityOpt = EOModelUtils.entityNamed(eomodel,entityName)
        println("eoFromJsonJsObject | entityOpt " + entityOpt)
        entityOpt match {
          case Some(entity) =>
            val pkNames = entity.pkAttributeNames
            println("eoFromJsonJsObject | pkNames " + pkNames)

            // Iterate on fields (Seq[(String, JsValue)]
            val eo = eoOpt match {
              case Some(x) => x
              case None =>
                val fieldsMap = jsObj.fields.toList.toMap

                val pk = fieldsMap("id") match {
                  case JsNumber(bigDecimal) => List(bigDecimal.toInt)
                  case JsArray(array) => {
                    array.map(pkv => pkv match {
                      case JsNumber(bigDecimal) => Some(bigDecimal.toInt)
                      case _ => None
                    }).toList.flatten
                  }
                }
                EOValue.dryEOWithEntity(entity.name, EOPk(pk))
            }
            val jObjValues: Seq[(String, JsValue)] = jsObj.fields.filter(x => !(pkNames.contains(x._1) || x._1.equals("type")|| x._1.equals("id")))
            println("jObjValues " + jObjValues)
            val valuesMap = valueMapWithJsonFields(eomodel, jObjValues)
            Some(EOValue.takeValuesForKeys(eo, valuesMap))


          case None =>
            println("no entity found for entityName: " + entityName)
            None
        }

      case None =>
        println("no type in json: " + jsObj)
        None
    }
  }

  // The EO Extrator will go inside all to-one relationship to see if there is some EO which are more than dry
  // The goal is to be able to register everything EO in the cache then
  def eoExtractor(eo : EO, accu : Set[EO]) : Set[EO] = {
    val values = eo.values
    val eoValueOpts = values.map(eov => eov match {
      case ObjectValue(eopk) => if (eo.values.isEmpty) None else Some(eo)
      case _ => None
    })
    val eos = eoValueOpts.flatten.toSet
    if (eos.isEmpty) {
      accu
    } else {
      //println("values " + values)
      val newAccu = accu ++ eos
      eos.flatMap(eo => eoExtractor(eo, newAccu))
    }
  }


  def convertEOJsonToEOs(eomodel: EOModel, eo: EO, resultBody: JsObject): List[EO] = {
    val jObj = resultBody.asInstanceOf[JsObject]
    val refreshedEOOpt = eoFromJsonJsObject(eomodel, Some(eo),jObj)
    refreshedEOOpt match {
      case Some(refreshedEO) =>
        eoExtractor(refreshedEO, Set(refreshedEO)).toList
      case None => List()
    }
  }

}

class EORepoActor  (eomodelActor: ActorRef, ws: WSClient) extends Actor with ActorLogging {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")



  // TBD Sorting parameter
  // qualifier=product.name='CMS' and parentProductReleases.customer.acronym='ECHO'&sort=composedName|desc
  def qualifiersUrlPart(q: EOQualifier): String = {
    val qualifiersStrings = q match {
      case EOAndQualifier(qualifiers) => qualifiers.map(q => qualifierUrlPart(q.asInstanceOf[EOKeyValueQualifier]))
      case _ => List()
    }
    return qualifiersStrings.mkString(" and ")
  }

  def qualifierUrlPart(qualifier: EOKeyValueQualifier): String = {
    val value = qualifier.value
    return value match {
      case StringValue(stringV) => qualifier.key + " caseInsensitiveLike '*" + stringV + "*'"
      case IntValue(i) => "" // TODO
      case BooleanValue(b) => qualifier.key + " = " + (if (b) "1" else "0")
      case ObjectValue(eo) => "" // TODO
      // To Restore case ObjectsValue(eos) => "" // TODO
    }
  }


  def searchOnWORepository(fs: EOFetchSpecification, missingKeys: List[String]): Future[List[EO]] = {
    val entityName = EOFetchSpecification.entityName(fs)
    val qualifier = fs match {
      case fa: EOFetchAll => None
      case fq: EOQualifiedFetch => Some(fq.qualifier)
    }
    searchOnWORepository(entityName, qualifier,missingKeys)
  }


  def searchOnWORepository(entityName: String, qualifierOpt: Option[EOQualifier], missingKeys: List[String]): Future[List[EO]] = {
    Logger.debug("Search with fs:" + entityName)
    val qualifierSuffix = qualifierOpt match {
      case Some(q) => "?qualifier=" + qualifiersUrlPart(q)
      case _ => ""
    }
    val missingKeysFormKeyStr =  missingKeysFormKeyString(missingKeys.toSet)
    val missingKeysAddon = if (missingKeysFormKeyStr.isEmpty) "" else "?" + missingKeysFormKeyStr

    val url = d2spaServerBaseUrl + "/" + entityName + ".json" + qualifierSuffix + missingKeysAddon
    Logger.debug("Search URL:" + url)
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>
      println("response " + response)

      val resultBody = response.json
      println("resultBody " + resultBody)

      val array = resultBody.asInstanceOf[JsArray]
      var eos = List[EO]()
      for (item <- array.value) {
        println("searchOnWORepository | item " + item)

        val valuesMap = valueMapWithJson(item)
        println("valuesMap " + valuesMap)
        //Logger.debug("valuesMap " + valuesMap)
        val pkEOValue = valuesMap("id")
        val pk = EOValue.juiceInt(pkEOValue)
        val dryEO = EOValue.dryEOWithEntity(entityName, EOPk(List(pk)))
        val eo = EOValue.takeValuesForKeys(dryEO,valuesMap)
        eos ::= eo
      }
      //Logger.debug("Search: eos created " + eos)

      eos
    }
  }

  def valueMapWithJson(jsval : JsValue): Map[String,EOValue] = {
    val jObj = jsval.asInstanceOf[JsObject]
    println("valueMapWithJson | jObj " + jObj)
    println("valueMapWithJson | jObj.fields " + jObj.fields)
    EORepoActor.valueMapWithJsonFields(eomodel, jObj.fields)
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

  def missingKeysFormKeyString(missingKeys: Set[String]) = {
    if (missingKeys.size > 0) {
      val toArrayString = missingKeys.map(x => s""""${x}"""").mkString("(", ",", ")")
      "missingKeys=" + toArrayString
    } else ""

  }

  def completeEO(eoFault: EOFault, missingKeys: Set[String]): Future[List[EO]] = {
    Logger.debug("Complete EO: " + eoFault + " missing keys: " + missingKeys)
    val entityName = eoFault.entityName
    val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
    entityOpt match {
      case Some(entity) =>
        val eo = EOValue.dryEOWithEntity(entity.name, eoFault.pk)
        val missingKeysFormKeyStr =  missingKeysFormKeyString(missingKeys)
        val pks = eoFault.pk
        pks.pks.size match {
          case 1 =>
            val pk = pks.pks.head
            // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Customer/2/propertyValues.json?missingKeys=("projects")
            val url = d2spaServerBaseUrl + "/" + entityName + "/" + pk + "/propertyValues.json?" + missingKeysFormKeyStr
            completeEOWithUrl(eo, url, x => x)

          case _ =>
            val propertyValues = entity.pkAttributeNames.zip(pks.pks)
            val qualStrings = propertyValues.map(pv => {
              pv._1 + "=" + pv._2
            })
            val qualString = qualStrings.mkString(" and ")

            val url = d2spaServerBaseUrl + "/" + entityName + ".json?qualifier=" + qualString + "&batchSize=-1&" + missingKeysFormKeyStr
            completeEOWithUrl(eo, url, x => x.asInstanceOf[JsArray].value.head)

        }




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

      // Another WS response:
      // {
      //   "id": 1536517,
      //   "type": "ActionEvent",
      //   "date": "2013-09-25T17:23:54Z",
      //   "text": "CMC",
      //   "responsible": {
      //     "id": 9,
      //     "type": "User"
      //   },
      //   "typeDescription": "Ftp Login changed to"
      // }

      case None =>
        Logger.error("Complete EO: error : " + "No entity found for name: " + entityName)

        val pseudoEOWithFault = EO(entityName, pk = eoFault.pk)
        Future(List(handleException("No entity found for name: " + eoFault.entityName, pseudoEOWithFault)))

    }
  }

  def completeEOWithUrl(eo: EO, url: String, unwrapper: JsValue => JsValue): Future[List[EO]] = {
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    Logger.debug("Complete EO: request : " + url)

    val futureResponse: Future[WSResponse] = request.get
    futureResponse.map { response =>
      try {
        val resultBody = unwrapper(response.json)
        val jObj = resultBody.asInstanceOf[JsObject]

        EORepoActor.convertEOJsonToEOs(eomodel, eo, jObj)

      } catch {
        case parseException: JsonParseException => {
          List(handleException(response.body, eo))
        }
        case t: Throwable => {
          List(handleException(t.getMessage(), eo))
        }

      }
    }

  }


  def hydrateEOs(entityName: String, pks: Seq[EOPk], missingKeys: Set[String]): Future[List[EO]] = {
    val futures = pks.map(pk => completeEO(EOFault(entityName, pk), missingKeys))
    val futureOfList = Future sequence futures
    futureOfList.map {
      x => x.flatten.toList
    }
  }

  // POST (create)
  // call Project.json with: {"descr":"bb","projectNumber":2}
  // {"id":2,"type":"Project","descr":"bb","projectNumber":2,"customer":null}

  def newEO(entityName: String, eo: EO): Future[EO] = {
    Logger.debug("New EO: " + eo)

    val url = d2spaServerBaseUrl + "/" + entityName + ".json"

    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)

    // For all value the user has set, we prepare the json to be sent
    val eoDefinedValues = EOValue.definedValues(eo)
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get

    Logger.debug("eoDefinedValues " + eoDefinedValues)
    val eoValues = eoDefinedValues map { case (key, valContainer) => (key, woWsParameterForValue(valContainer, entity, key)) }
    Logger.debug("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    Logger.debug("Upate WS:  " + url)
    Logger.debug("WS post data: " + data)

    // Execute the POST
    val futureResponse: Future[WSResponse] = request.post(data)

    // Work with the response: The goal is to update the PK that could have been generated by the server
    // (for single PK only because the multiple pk are reference to other object so we know them already)
    // But we don't set the pk eo attribute yet because the client needs to find it's chicks
    futureResponse.map { response =>
      try {

        val pkAttributeNames = entity.pkAttributeNames
        pkAttributeNames.size match {
          case 1 =>
            val pkAttributeName = pkAttributeNames.head
            val resultBody = response.json
            val jsObj = resultBody.asInstanceOf[JsObject]

            val pkValue = jsObj.value("id")
            pkValue match {
              case JsNumber(pkNumber) =>
                // We don't set the pk of the EO here because the client has to identify it. It's going to be its responsibility
                // TODO this contradicts the fact that pk should not be in the value in order to test if it is empty
                val pkIntValue = pkNumber.intValue()
                EOValue.takeValueForKey(eo, EOValue.intV(pkIntValue), pkAttributeName)
              case _ => eo // should never occur but let's have it as a safety nest
            }

          case _ =>
            eo
        }

      } catch {
        case parseException: JsonParseException => {
          handleException(response.body, eo)
        }
        case t: Throwable => {
          handleException(t.getMessage(), eo)
        }
      }
    }
  }


  // 1) go through the tree and save all to-one  starting from leaves:
  //    while any children, go to it,
  //      if is leaf or only to-many children and coming from to-one or to-many flattened > if not saved, save, flag saved then backward and recurse
  //    if not saved, save root

  // How to detect flattened to-many ?
  //
  def savedEOsForToOne(eos: List[EO]) : List[EO] = {
    val root = eos.head
    val entityName = root.entityName
    val pkByEO = eos.map(eo => (eo.pk, eo)).toMap
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    val relationships = entity.relationships
    //val filteredRelatinships = relationships.filter(r => {!r.isToMany && EORelationship.isFlattened()})
    for (relationship <- relationships) {

    }

    eos
  }


  def savingEOTree(eos: List[EO]) : List[EO] = {
    traverseTree(eos.head, eos)
  }
  def traverseTree(root: EO, eos: List[EO]): List[EO] = {
    val entityName = root.entityName
    val pkByEO = eos.map(eo => (eo.pk, eo)).toMap
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    val relationships = entity.relationships
    val filteredRelationships = relationships.filter(rel => {root.keys.contains(rel.name)})
    val subEOLists: List[List[EO]] = filteredRelationships.map( relationship => {
        relationship.definition match {
          case Some(definition) =>
            // we want to consider flattened many-to-many
            // in such case the object around the link table will have non compound pk
            val rootPk = root.pk.pks.head
            val eoValueOpt = EOValue.valueForKey(root, relationship.name)
            eoValueOpt match {
              case ObjectsValue(eoPks: List[EOPk]) =>
                val keyPathFirstKey = EORelationship.keyPathFirstKey(definition)
                val linkRelationshipOpt = EORelationship.relationshipNamed(relationships, keyPathFirstKey)
                linkRelationshipOpt match {
                  case Some(linkRelationship) =>
                    val linkEntity = EOModelUtils.entityNamed(eomodel, linkRelationship.destinationEntityName).get
                    val newEOs = eoPks.map(eoPk => {
                      val isRootFirst = linkEntity.relationships.head.destinationEntityName.equals(entityName)
                      val destinationPk = eoPk.pks.head
                      val linkPks = if (isRootFirst) List(rootPk, destinationPk) else List(destinationPk, rootPk)
                      EO(entityName = linkEntity.name, pk = EOPk(linkPks))
                    })
                    

                    newEOs
                  case None =>
                    List.empty[EO]
                }
              case _ => List.empty[EO]
            }

          case None =>
            val eoValueOpt = EOValue.valueForKey(root, relationship.name)
            eoValueOpt match {
              case Some(eovalue) =>
                eovalue match {
                  case ObjectValue(eoPk: EOPk) =>
                    if (pkByEO.contains(eoPk)) {
                      List(pkByEO(eoPk))
                    } else List.empty[EO]
                  case  ObjectsValue(eoPks: List[EOPk]) =>
                    val eoOpts = eoPks.map( eoPk  => {
                      if (pkByEO.contains(eoPk)) {
                        Some(pkByEO(eoPk))
                      } else None
                    })
                    eoOpts.flatten
                  case _ => List.empty[EO]
                }
              case None => List.empty[EO]
            }
        }
    })
    val subEOs = subEOLists.flatten
    root :: subEOs
  }

  // Upate WS:  http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/3.json
  //  WS post data: {"descr":"1","id":3,"customer":{"id":2,"type":"Customer"},"projectNumber":3,"type":"Project"}

  // traverse the tree 2 times:
  // 1) go through the tree and save all to-one  starting from leaves:
  //    while any children, go to it,
  //      if is leaf or only to-many children and coming from to-one or to-many flattened > if not saved, save, flag saved then backward and recurse
  //    if not saved, save root
  // 2) go through the tree and save all destination of to-many:
  //    while any children, go to it,
  //      if destination of a to many > if not saved, save, flag saved and recurse
  def updateEO(eos: List[EO]): Future[List[EO]] = {
    val savingEOs = savingEOTree(eos)

    val tree = savedEOsForToOne(savingEOs)

    /*val eos = extractEOsToSave(eo)
    val updateFutures: List[Future[EO]] = eos.map(eo => saveEO(eo))
    val futureOfList = Future sequence updateFutures
    futureOfList*/
    //saveEO(eo)
    Future(tree)
  }

  def extractEOsToSave(eo: EO): List[EO] = {
    List()
  }

  def saveEO(eo: EO): Future[EO] = {
    Logger.debug("Update EO: " + eo)
    val pk = eo.pk.pks.head

    val entityName = eo.entityName
    val url = d2spaServerBaseUrl + "/" + entityName + "/" + pk + ".json"

    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)
    /*val eoDefinedValues = eo.values filter (v => {
      Logger.debug("v._2" + v._2)
      EOValueUtils.isDefined(v._2) })*/
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get

    Logger.debug("eoDefinedValues " + eo.values)
    val eoValues = EOValue.keyValues(eo) map { case (key, valContainer) => (key, woWsParameterForValue(valContainer, entity, key)) }
    Logger.debug("eoValues " + eoValues)
    val data = Json.toJson(eoValues)

    Logger.debug("Upate WS:  " + url)
    Logger.debug("WS post data: " + data)

    val futureResponse: Future[WSResponse] = request.put(data)
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        val array = resultBody.asInstanceOf[JsObject]
        println("updateEO Saved EO " + eo)
        eo
      } catch {
        case parseException: JsonParseException => {
          println("updateEO | JsonParseException " + parseException)
          handleException(response.body, eo)
        }
        case t: Throwable => {
          println("updateEO | Throwable " + t)
          handleException(t.getMessage(), eo)
        }
      }
    }
  }


  // DELETE
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/2.json
  // => 2

  // Return the EO with everything and may be a validationError
  def deleteEO(eo: EO): Future[EO] = {
    val pk = eo.pk.pks.head
    val url = d2spaServerBaseUrl + "/" + eo.entityName + "/" + pk + ".json"
    Logger.debug("Delete EO: " + eo)
    Logger.debug("Delete WS: " + url)

    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.delete()
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        eo
      } catch {
        case parseException: JsonParseException => {
          handleException(response.body, eo)
        }
        case t: Throwable => {
          handleException(t.getMessage(), eo)
        }
      }
    }
  }

  def pkAttributeNamesForEntityNamed(entityName: String): List[String] = {
    val entityOpt = eomodel.entities.find(e => e.name.equals(entityName))
    entityOpt match {
      case Some(entity) => entity.pkAttributeNames
      case None => List()
    }
  }

  // We don't link a eo with an destination eo which has compound pks
  // Instead we create the destination eo which will have only to-one to single pk eo
  // -> we don't support compound pks eo here
  def woWsParameterForValue(value: EOValue, entity: EOEntity, key: String): JsValue = {
    value match {
      case StringValue(stringV) => JsString(stringV)

      case IntValue(intV) => JsNumber(intV)

      case ObjectValue(eo) => {
        val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, key).get

        val entityName = destinationEntity.name
        val pk = eo.pks.head
        JsObject(Seq("id" -> JsNumber(pk), "type" -> JsString(entityName)))
      }
      case EmptyValue => JsNull
      case _ => JsString("")
    }

  }


  def handleException(error: String, eo: EO): EO = {
    Logger.debug("error " + error + " with eo " + eo)

    eo.copy(validationError = Some(error))
  }

  var eomodel: EOModel = null

  override def preStart {
    println("Menus Actors: preStart")
    eomodelActor ! GetEOModel(None, self)
  }

  def receive = LoggingReceive {
    case EOModelResponse(model, d2wContext) =>
      eomodel = model

     // for a D2WContext, give ruleResults for
     // - displayNameForEntity
     // - displayPropertyKeys
     // and for each displayPropertyKeys:
     // - displayNameForProperty
     // - componentName
     // - attributeType

    /*case HydrateAll(fs: EOFetchAll, requester: ActorRef) =>
      log.debug("SearchAll")
      val entityName = EOFetchSpecification.entityName(fs)

      searchOnWORepository(entityName, None, List().map(rrs =>
        requester ! FetchedObjects(entityName, rrs, None)
      )*/

    /*case Search(fs: EOFetchSpecification, requester: ActorRef) =>
      log.debug("Search")

      searchOnWORepository(fs).map(rrs =>
        requester ! FetchedObjectsForList(fs,rrs)
      )*/

    /*case CompleteEO(eo: EOFault, missingKeys: Set[String], requester: ActorRef) =>
      log.debug("Get GetRulesForMetaData")
      completeEO(eo,missingKeys).map(rrs =>
        requester ! FetchedObjects(eo.entityName, rrs)
      )*/
    case HydrateEOs(entityName: String, pks: Seq[EOPk], missingKeys: Set[String], ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      log.debug("Get HydrateEOs")
      hydrateEOs(entityName, pks, missingKeys).map(rrs =>
        requester ! FetchedObjects(entityName, rrs, ruleResults)
      )


    case Hydrate(d2wContextOpt, hydration: Hydration, ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      log.debug("Get Hydrate")
      hydration.drySubstrate match {
        case DrySubstrate(_, Some(eoFault), _) =>
          val missingKeys = RulesUtilities.missingKeysWith(hydration.wateringScope, ruleResults)
          println("Hydrate missingKeys " + missingKeys)
          println("Hydrate hydration.wateringScope " + hydration.wateringScope)

          hydrateEOs(eoFault.entityName, List(eoFault.pk), missingKeys.toSet).map(rrs => {
            println("Hydrate gives " + rrs)
            println("Hydrate send to " + requester)
            requester ! CompletedEOs(d2wContextOpt, hydration, rrs, ruleResults)
          }
          )
        case DrySubstrate(_, _, Some(fs)) =>
          val missingKeys = RulesUtilities.missingKeysWith(hydration.wateringScope, ruleResults)
          println("Hydrate fs : " + fs)
          println("Hydrate with missingKeys " + missingKeys)
          searchOnWORepository(fs, missingKeys).map(rrs =>
            requester ! CompletedEOs(d2wContextOpt, hydration, rrs, ruleResults)
          )

      }




    case NewEO(d2wContext: D2WContext, eo: EO, ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      log.debug("Get NewEO " + EO)
      val entityName = d2wContext.entityName.get
      newEO(entityName, eo).map(rrs =>
        requester ! SavingResponse(d2wContext, List(rrs), ruleResults)
      )
    case UpdateEO(d2wContext: D2WContext, eos: List[EO], ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      log.debug("Get UpdateEO")
      updateEO(eos).map(rrs =>
        requester ! SavingResponse(d2wContext, rrs, ruleResults)
      )


    case DeleteEO(eo: EO, requester: ActorRef)  =>
      log.debug("Get GetRulesForMetaData")
      deleteEO(eo).map(rrs =>
        requester ! DeletingResponse(rrs)
      )

  }


}

