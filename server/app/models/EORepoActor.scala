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
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import upickle.default.{macroRW, ReadWriter => RW}
import upickle.default.write
import play.api.libs.json.Reads._
import sbt.testing.NestedSuiteSelector
import utils.EOUtils
import play.api.libs.json


trait LagomGetObj

case class Customer (
                      trigram: String,
                      customerID: Long,
                      name: String,
                      customerType: String,
                      dynamicsAccountID: Option[String],
                      headCountry: String,
                      region: String
                    ) extends LagomGetObj

object Customer {
  implicit def rw: RW[Customer] = macroRW
}

case class CustomerForCreation (
                      trigram: String,
                      customerID: Long,
                      name: String,
                      customerType: String,
                      dynamicsAccountID: String,
                      headCountry: String,
                      region: String
                    ) extends LagomGetObj

object CustomerForCreation {
  implicit def rw: RW[CustomerForCreation] = macroRW
}



case class Country (
                     countryID: Long,
                     flagImage: String,
                     isoCode: String,
                     name: String,
                     region: String
                    ) extends LagomGetObj

object Country{
  implicit def rw: RW[Country] = macroRW
}


object EORepoActor {
  def props(eomodelActor: ActorRef, ws: WSClient): Props = Props(new EORepoActor(eomodelActor, ws))


  //case class SearchAll(fs: EOFetchAll, requester: ActorRef) //: Future[Seq[EO]]
  case class HydrateAll(fs: EOFetchAll, requester: ActorRef) //: Future[Seq[EO]]

  case class ImportCustomers(requester: ActorRef)
  case class ImportCountries(requester: ActorRef)
  case class ImportCustomersBackup(requester: ActorRef)
  case class ImportCustomer(requester: ActorRef, istCustomers: List[CustomerForCreation])
  case class ImportCountry(requester: ActorRef, restOfEOs: List[EOContaining])
  //case class Search(fs: EOFetchSpecification, requester: ActorRef) //: Future[Seq[EO]]

  //case class CompleteEO(d2wContext: D2WContext, eo: EOFault, missingKeys: Set[String], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]
  case class HydrateEOs(entityName: String, pks: Seq[EOPk], missingKeys: Set[String], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[Seq[EO]]
  case class Hydrate(d2wContextOpt: Option[D2WContext], hydration: Hydration, ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[Seq[EO]]

  case class NewEO(d2wContext: D2WContext, eos: List[EOContaining], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]
  case class UpdateEO(d2wContext: D2WContext, eos: List[EOContaining], ruleResults: Option[List[RuleResult]], requester: ActorRef) //: Future[EO]

  case class DeleteEO(eo: EOContaining, requester: ActorRef) // : Future[EO]

  // Responses
  case class FetchedObjects(entityName: String, eos: List[EOContaining], ruleResults: Option[List[RuleResult]])

  case class CompletedEOs(d2wContext: Option[D2WContext], hydration: Hydration, eo: List[EOContaining], ruleResults: Option[List[RuleResult]])

  //case class FetchedObjectsForList(fs: EOFetchSpecification, eos: List[EO])

  case class SavingResponse(d2wContext: D2WContext, eos: List[EOContaining], ruleResults: Option[List[RuleResult]])

  case class DeletingResponse(eo: EOContaining)




  // The EO Extrator will go inside all to-one relationship to see if there is some EO which are more than dry
  // The goal is to be able to register everything EO in the cache then
  def eoExtractor(eo: EO, accu: Set[EO]): Set[EO] = {
    val values = eo.values
    val eoValueOpts = values.map(eov => eov match {
      case ObjectValue(eopk) => if (eo.values.isEmpty) None else Some(eo)
      case _ => None
    })
    val eos = eoValueOpts.flatten.toSet
    //println(" eos " + eos)
    if (eos.isEmpty) {
      accu
    } else {
      //println("values " + values)
      val newAccu = accu ++ eos
      eos.flatMap(eo => eoExtractor(eo, newAccu))
    }
  }
  //


}

class EORepoActor(eomodelActor: ActorRef, ws: WSClient) extends Actor with ActorLogging {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")

  implicit lazy val lagomCustomerReads: Reads[Customer] = (
    (JsPath \ "trigram").read[String] and
      (JsPath \ "customerID").read[Long] and
      (JsPath \ "name").read[String] and
      (JsPath \ "customerType").read[String] and
      (JsPath \ "dynamicsAccountID").readNullable[String] and
      (JsPath \ "headCountry").read[String] and
      (JsPath \ "region").read[String]
    ) (Customer.apply _)

  implicit lazy val lagomCustomerForCreationReads: Reads[CustomerForCreation] = (
    (JsPath \ "trigram").read[String] and
      (JsPath \ "customerID").read[Long] and
      (JsPath \ "name").read[String] and
      (JsPath \ "customerType").read[String] and
      (JsPath \ "dynamicsAccountID").read[String] and
      (JsPath \ "headCountry").read[String] and
      (JsPath \ "region").read[String]
    ) (CustomerForCreation.apply _)

  implicit lazy val lagomCountryReads: Reads[Country] = (
      (JsPath \ "countryID").read[Long] and
      (JsPath \ "flagImage").read[String] and
      (JsPath \ "isoCode").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "region").read[String]
    ) (Country.apply _)


  // search knows which repo to fetch from but we can force using WO with forceWODataSource = true
  def search(fs: EOFetchSpecification, missingKeys: List[String], forceWODataSource : Boolean): Future[List[EOContaining]] = {
    val entityName = EOFetchSpecification.entityName(fs)
    val qualifier = fs match {
      case fa: EOFetchAll => None
      case fq: EOQualifiedFetch => Some(fq.qualifier)
    }

    val isLagomQuery = entityName.equals("Customer") || entityName.equals("Country")
    val queryWO = forceWODataSource || !isLagomQuery

    // If Customer or Country, we can fetch WO only if force
    if (queryWO){
      println("Search WO")
      searchOnWORepository(entityName, qualifier, missingKeys)
    } else{
      println("Search Lagom")
      searchOnLagom(entityName, qualifier)
    }
  }

  def paramStringWithParams(params: List[String]): String = {
    if (params.size == 0) {
      ""
    } else {
      val head = params.head
      val tail = params.tail
      val tailString = if (tail.size == 0) "" else "&" + tail.mkString("&")
      "?" + head + tailString
    }
  }



  def convertLagomCountryToWOCountryJson(country: Country) : JsValue = {
    val region = Map(
      "id" -> JsNumber(3),
      "type" -> JsString("Region")
    )

    val valuesMap = Map (
      "id" -> JsNumber(country.countryID),
      "flagPath" -> JsString(country.flagImage),
      "isoCode" -> JsString(country.isoCode),
      "name" -> JsString(country.name),
      "region" -> Json.toJson(region),
    )
    Json.toJson(valuesMap)
  }


  def convertLagomCustomerToCustomerContainer(lagomGetObj: LagomGetObj, countryByIsoCode: Map[String, List[CountryContainer]]): Option[CustomerContainer] = {
    lagomGetObj match {
      case customer: Customer =>
        val entityName = "Customer"
        val customerEO = EOValue.dryEOWithEntity(entityName, EOPk(List(customer.customerID.toInt)))
        val country = countryByIsoCode(customer.headCountry).head
        Some(CustomerContainer(customer.trigram, customer.name, customer.customerType, customer.dynamicsAccountID, country, customer.region, customerEO))
      case _ => None
    }
  }

  def convertLagomCountryToCountryContainer(lagomGetObj: LagomGetObj) : Option[CountryContainer] = {
    lagomGetObj match {
      case country: Country =>
        val entityName = "Customer"
        val eo = EOValue.dryEOWithEntity(entityName, EOPk(List(country.countryID.toInt)))
        Some(CountryContainer(country.flagImage, country.isoCode, country.name, country.region, eo))
      case _ => None
    }
  }


  def convertLagomCustomerToWOCustomerJson(customer: Customer) : JsValue = {
    val country = Map(
      "id" -> JsNumber(3),
      "type" -> JsString("Country")
    )
    val region = Map(
      "id" -> JsNumber(3),
      "type" -> JsString("Region")
    )
    val manufacturer = Map(
      "id" -> JsArray(Seq(JsNumber(1), JsNumber(customer.customerID))),
      "type" -> JsString("CustomerBusiness")
    )


    val valuesMap = Map (
      "customerTrigram" -> JsString(customer.trigram),
      "id" -> JsNumber(customer.customerID),
      "preferedName" -> JsString(customer.name),
      "isOperatorNumber" -> JsBoolean(customer.customerType.equals("Operator")),
      "dynamicsAccountID" -> (customer.dynamicsAccountID match {
        case Some(id) => JsString(id)
        case None => JsNull
      }),
      "headCountry" -> Json.toJson(country),
      "region" -> Json.toJson(region),
      "manufacturer" -> Json.toJson(manufacturer),
      "sites" -> JsArray()
    )
    Json.toJson(valuesMap)
  }

  def searchOnLagom(entityName: String, qualifierOpt: Option[EOQualifier]): Future[List[EOContaining]] = {
    entityName match {
      case "Customer" =>
        val result = rawSearchOnLagom("Country", None).map(countries => {
          val countryCountainerOpts = countries.map(convertLagomCountryToCountryContainer(_))
          val countryCountainers = countryCountainerOpts.flatten
          val countryByIsoCode =  countryCountainers.groupBy(c => c.isoCode)

          val toto = rawSearchOnLagom("Customer", None).map(customers => {
            val custContainterOpts = customers.map( custLagomObj => {

              convertLagomCustomerToCustomerContainer(custLagomObj, countryByIsoCode)
            })
            custContainterOpts.flatten
          })
          toto
        })
        result.flatten
      case "Country" =>
        val toto = rawSearchOnLagom(entityName, None).map(custLagomObjs => {
          val titi = custLagomObjs.map(convertLagomCountryToCountryContainer(_))
          titi.flatten
        })
        toto
    }
  }


  //Some(convertLagomCountryToCountryContainer(country))
//                Some(convertLagomCustomerToCustomerContainer(customer))

  def rawSearchOnLagom(entityName: String, qualifierOpt: Option[EOQualifier]): Future[List[LagomGetObj]] = {

    val lagomrEntity = entityName match {
      case "Customer" => "customer"
      case "Country" => "country"
      case _ => "customer"
    }
    val url = "http://localhost:9000/api/" + lagomrEntity
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>
      val resultBody = response.json
      val array = resultBody.asInstanceOf[JsArray]
      val lagomGetObjs: List[Option[LagomGetObj]] = array.value.map (item => {
        //println("searchOnWORepository | item " + item)
        //Logger.debug(menuRaw)
         entityName match {
          case "Country" =>
            val obj = item.validate[Country]
            obj match {
              case s: JsSuccess[Country] =>
                Some(s.get)

              case e: JsError =>
                Logger.error("Errors: " + JsError.toJson(e).toString())
                None
            }
          case _ =>
            val obj = item.validate[Customer]
            obj match {
              case s: JsSuccess[Customer] =>
                Some(s.get)

              case e: JsError =>
                Logger.error("Errors: " + JsError.toJson(e).toString())
                None
            }
        }
      }).toList
      lagomGetObjs.flatten
    }
  }

    // http://127.0.0.1:1445/cgi-bin/WebObjects/CustomerBackend.woa/ra/Customer/345/propertyValues.json?missingKeys=(%22manufacturer%22,%22sites%22)
/*
{
    "id": 303,
    "type": "Customer",
    "sites": [
        {
            "id": 186,
            "type": "Site"
        },
        {
            "id": 470,
            "type": "Site"
        }
    ],
    "manufacturer": {
        "id": [
            1,
            303
        ],
        "type": "CustomerBusiness"
    }
}
 */

  def woUrlWith(entityName: String, qualifierOpt: Option[EOQualifier], missingKeys: List[String]) = {
    println("qualifierOpt " + qualifierOpt)
    val qualifierSuffixOpt = qualifierOpt match {
      case Some(q) => Some("qualifier=" + UrlUtils.qualifiersUrlPart(q))
      case _ => None
    }
    val missingKeysFormKeyStr = missingKeysFormKeyString(missingKeys.toSet)
    val missingKeysAddonOpt = if (missingKeysFormKeyStr.isEmpty) None else Some(missingKeysFormKeyStr)

    val params = List(qualifierSuffixOpt,missingKeysAddonOpt).flatten
    val paramStr = paramStringWithParams(params)

    d2spaServerBaseUrl + "/" + entityName + ".json" + paramStr
  }


  def searchOnWORepository(entityName: String, qualifierOpt: Option[EOQualifier], missingKeys: List[String]): Future[List[EOContaining]] = {
    //Logger.debug("Search with fs:" + entityName)

    val url = woUrlWith(entityName, qualifierOpt, missingKeys)

    println("Search URL:" + url)
    Logger.debug("Search URL:" + url)
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>
      //println("response " + response)

      val resultBody = response.json
      //println("resultBody " + resultBody)

      val array = resultBody.asInstanceOf[JsArray]
      var eos = List[EOContaining]()
      for (item <- array.value) {
        //println("searchOnWORepository | item " + item)

        val valuesMap = valueMapWithJson(item)
        //println("valuesMap " + valuesMap)
        //Logger.debug("valuesMap " + valuesMap)
        val pkEOValue = valuesMap("id")
        val pk = EOValue.juiceInt(pkEOValue)
        val dryEO = EOValue.dryEOContainerWithEntity(entityName, EOPk(List(pk)))
        val eo = EOValue.takeValuesForKeys(dryEO, valuesMap)
        eos ::= eo
      }
      //Logger.debug("Search: eos created " + eos)

      eos
    }
  }

  def valueMapWithJson(jsval: JsValue): Map[String, EOValue] = {
    val jObj = jsval.asInstanceOf[JsObject]
    //println("valueMapWithJson | jObj " + jObj)
    //println("valueMapWithJson | jObj.fields " + jObj.fields)
    EOUtils.valueMapWithJsonFields(eomodel, jObj.fields)
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

  def completeEO(eoFault: EOFault, missingKeys: Set[String]): Future[List[EOContaining]] = {
    Logger.debug("Complete EO: " + eoFault + " missing keys: " + missingKeys)
    val entityName = eoFault.entityName
    val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
    entityOpt match {
      case Some(entity) =>
        val eoContaining = EOValue.dryEOContainerWithEntity(entity.name, eoFault.pk)
        val missingKeysFormKeyStr = missingKeysFormKeyString(missingKeys)
        val pks = eoFault.pk
        pks.pks.size match {
          case 1 =>
            val pk = pks.pks.head
            // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Customer/2/propertyValues.json?missingKeys=("projects")
            val url = d2spaServerBaseUrl + "/" + entityName + "/" + pk + "/propertyValues.json?" + missingKeysFormKeyStr
            completeEOWithUrl(eoContaining, url, x => x)

          case _ =>
            val propertyValues = entity.pkAttributeNames.zip(pks.pks)
            val qualStrings = propertyValues.map(pv => {
              pv._1 + "=" + pv._2
            })
            val qualString = qualStrings.mkString(" and ")

            val url = d2spaServerBaseUrl + "/" + entityName + ".json?qualifier=" + qualString + "&batchSize=-1&" + missingKeysFormKeyStr
            completeEOWithUrl(eoContaining, url, x => x.asInstanceOf[JsArray].value.head)

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

        val pseudoEOWithFault = EOValue.dryEOContainerWithEntity(entityName, eoFault.pk)
        Future(List(handleException("No entity found for name: " + eoFault.entityName, pseudoEOWithFault)))

    }
  }

  def completeEOWithUrl(eoContaining: EOContaining, url: String, unwrapper: JsValue => JsValue): Future[List[EOContaining]] = {
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    //Logger.debug("Complete EO: request : " + url)

    val futureResponse: Future[WSResponse] = request.get
    futureResponse.map { response =>
      try {
        val resultBody = unwrapper(response.json)
        val jObj = resultBody.asInstanceOf[JsObject]

        //println("completeEOWithUrl | body | jObj " + jObj)
        val res = EOUtils.convertEOJsonToEOs(eomodel, eoContaining, jObj)
        //println("completeEOWithUrl | convertEOJsonToEOs | res " + res)
        res

      } catch {
        case parseException: JsonParseException => {
          List(handleException(response.body, eoContaining))
        }
        case t: Throwable => {
          List(handleException(t.getMessage(), eoContaining))
        }

      }
    }

  }


  def hydrateEOs(entityName: String, pks: Seq[EOPk], missingKeys: Set[String]): Future[List[EOContaining]] = {
    val futures: Seq[Future[List[EOContaining]]] = pks.map(pk => completeEO(EOFault(entityName, pk), missingKeys))
    val futureOfList = Future sequence futures
    futureOfList.map {
      x => x.flatten.toList
    }
  }


  // 2) go through the tree and save all destination of to-many:
  //    while any children, go to it,
  //      if destination of a to many > if not saved, save, flag saved and recurse
  def savedEOsForToMany(eos: List[EOContaining]): List[Future[EOContaining]] = {
    //Logger.debug("savedEOsForToMany | eos " + eos)
    val eosMap = eosByEntityNameByPk(eos)
    val result = traverseTreeToMany(eos.head, eosMap, true, None, None)
    //Logger.debug("savedEOsForToMany | result " + result)
    result
  }


  // Save first
  def traverseTreeToMany(root: EOContaining, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]], toSave: Boolean, relationshipOpt: Option[EORelationship], sourceEOOpt: Option[EOContaining]): List[Future[EOContaining]] = {
    //Logger.debug("traverseTreeToMany | root " + root + " relationship " + relationshipOpt + " source EO " + sourceEOOpt)

    val savedRoot: EOContaining = if (toSave) {
      // Update reverse to-one pk of the coming from relationship
      val toSaveEO = relationshipOpt match {

        case Some(relationship) =>
          // val inverseRelationshipOpt = EORelationship.inverseRelationship(eomodel, relationship)

          // 1) Update Root with coming from relationship source EO which could have been saved (This in order to save root with correct references)
          //   Why no store the inverse relationship in the eomodel and save it like this in the play application, offering a tool to create it from WO Application.
          //     a) ask for values of relationship joins source attribute in source eo
          //     b) update root relationship joins destination attribute

          // 2) Update the ObjectsValue in the relationship source EO
          // Not necessary because not used in the save ??
          val sourceEO = sourceEOOpt.get

          val updateTupleOpts = relationship.joins.map(join => {
            val sourceValueOpt = EOValue.valueForKey(sourceEO, join.sourceAttributeName)
            val opt: Option[(String, EOValue)] = sourceValueOpt match {
              case Some(sourceValue) =>
                val key = join.sourceAttributeName
                Some((key, sourceValue))
              case None => None
            }
            opt
          })

          val updateTuples = updateTupleOpts.flatten
          val updateMap = updateTuples.toMap
          //Logger.debug("traverseTreeToMany | updateMap " + updateMap)


          val sourceEOEntity = EOModelUtils.entityNamed(eomodel, sourceEO.eo.entityName).get
          val sourcePkAttributeNames = sourceEOEntity.pkAttributeNames
          //Logger.debug("traverseTreeToMany | sourcePkAttributeNames " + sourcePkAttributeNames)

          val orderedPkOpts = sourcePkAttributeNames.map(key => if (updateMap.contains(key)) {
            val value = updateMap(key)
            value match {
              case IntValue(pk) => Some(pk)
              case _ => None
            }
          } else None)
          //Logger.debug("traverseTreeToMany | orderedPkOpts " + orderedPkOpts)

          val allDefined = orderedPkOpts.find(_.isEmpty).isEmpty
          if (allDefined) {
            val inverseRelationshipOpt = EORelationship.inverseRelationship(eomodel, sourceEOEntity, relationship)
            //Logger.debug("traverseTreeToMany | inverseRelationshipOpt of " + relationship.name + " =  " + inverseRelationshipOpt)
            inverseRelationshipOpt match {
              case Some(inverseRelationship) =>
                //Logger.debug("traverseTreeToMany | updateTupleOpts " + orderedPkOpts)
                val pks: List[Int] = orderedPkOpts.flatten
                val objectValue = ObjectValue(EOPk(pks))
                //Logger.debug("traverseTreeToMany | objectValue " + objectValue)
                val updatedRoot = EOValue.takeValueForKey(root, objectValue, inverseRelationship.name)
                //Logger.debug("traverseTreeToMany | updatedRoot " + updatedRoot)
                updatedRoot

              case None =>
                root
            }


          } else {
            root
          }
        case None =>
          root
      }

      Await result(saveEO(toSaveEO), 2 seconds)

    } else root
    //Logger.debug("traverseTreeToMany | savedRoot " + savedRoot)

    val entityName = savedRoot.eo.entityName
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    val relationships = entity.relationships

    // only the relationships of the EO Keys to optimize
    val filteredRelationships = relationships.filter(rel => {
      savedRoot.eo.keys.contains(rel.name)
    })

    // for each relationship we go down the tree
    val subEOLists = filteredRelationships.map(relationship => {
      //Logger.debug("traverseTreeToMany | for each relationship we go down the tree " + relationship.name)
      val eoValueOpt = EOValue.valueForKey(savedRoot, relationship.name)
      eoValueOpt match {
        case Some(eovalue) =>
          eovalue match {
            case ObjectValue(eoPk: EOPk) =>
              traverseToManyDestinationEOWithPk(eoPk, relationship.destinationEntityName, eosByEntityNameByPk)
            case ObjectsValue(eoPks: List[EOPk]) =>
              traverseToManyDestinationEOsWithPks(eoPks, relationship.destinationEntityName, eosByEntityNameByPk, relationship, savedRoot)
            case _ => List.empty[Future[EOContaining]]
          }
        case None => List.empty[Future[EOContaining]]
      }
    })
    val subEOs = subEOLists.flatten
    Future(savedRoot) :: subEOs
  }

  def traverseToManyDestinationEOWithPk(eoPk: EOPk, destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]) = {
    val eoOpt = eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    eoOpt match {
      case Some(eo) => traverseTreeToMany(eo, eosByEntityNameByPk, false, None, None)
      case None => List.empty[Future[EOContaining]]
    }

  }

  def traverseToManyDestinationEOsWithPks(eoPks: List[EOPk], destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]], relationship: EORelationship, sourceEO: EOContaining) = {
    val eoOpts = eoPks.map(eoPk => {
      eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    })
    val newEOs = eoOpts.flatten
    newEOs.map(traverseTreeToMany(_, eosByEntityNameByPk, true, Some(relationship), Some(sourceEO))).flatten
  }


  // 1) go through the tree and save all to-one  starting from leaves:
  //    while any children, go to it,
  //      if is leaf or only to-many children and coming from to-one or to-many flattened > if not saved, save, flag saved then backward and recurse
  //    if not saved, save root

  // How to detect flattened to-many ?
  //
  def savedEOsForToOne(eos: List[EOContaining]): List[Future[EOContaining]] = {
    val eosMap = eosByEntityNameByPk(eos)
    val result = traverseTreeToOne(eos.head, eosMap, true)
    //Logger.debug("savedEOsForToOne | Did save EOs for to-one " + result)
    result
  }

  case class ToManyUpdate(relationship: EORelationship, eovalue: ObjectsValue, eos: Map[EOPk, EOContaining])

  case class ToOneUpdate(relationship: EORelationship, oldEOPk: EOPk, eovalue: ObjectValue, eo: EOContaining)


  def updateDestinationEO(eoContaining: EOContaining, relationship: EORelationship): Option[(EOPk, EOContaining)] = {
    val eoValueOpt = EOValue.valueForKey(eoContaining, relationship.name)
    eoValueOpt match {
      case Some(eovalue) =>
        eovalue match {
          case ObjectValue(eoPk: EOPk) =>

            val updateTupleOpts = relationship.joins.map(join => {
              val sourceValueOpt = EOValue.valueForKey(eoContaining, join.destinationAttributeName)
              val opt: Option[(String, EOValue)] = sourceValueOpt match {
                case Some(sourceValue) =>
                  val key = join.sourceAttributeName
                  Some((key, sourceValue))
                case None => None
              }
              opt
            })
            val updateTuples = updateTupleOpts.flatten
            val updateMap = updateTuples.toMap
            Some((eoPk, EOValue.takeValuesForKeys(eoContaining, updateMap)))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def updateCacheWith(cache: Map[String, Map[EOPk, EOContaining]], partialUpdatedCache: Map[String, Map[EOPk, EOContaining]]) = {
    cache.keys.toList.map(entityName => {
      val eosByPK: Map[EOPk, EOContaining] = cache(entityName)
      val eopks = eosByPK.keys.toList
      eopks.map(eopk => {
        val anyUpdate = eoFromCache(partialUpdatedCache, entityName, eopk)
        anyUpdate match {
          case Some(ueo) => ueo
          case None => eosByPK(eopk)
        }
      })
    }).flatten
  }

  def traverseTreeToOne(rootContaining: EOContaining, eosByENameByPk: Map[String, Map[EOPk, EOContaining]], isFromToOne: Boolean): List[Future[EOContaining]] = {
    //Logger.debug("traverseTreeToOne | eosByENameByPk " + eosByENameByPk)

    val root = rootContaining.eo
    val entityName = root.entityName
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    val relationships = entity.relationships

    // only the relationships of the EO Keys to optimize
    val filteredRelationships = relationships.filter(rel => {
      root.keys.contains(rel.name)
    })

    //Logger.debug("traverseTreeToOne | filteredRelationships " + filteredRelationships)

    // Example: Saving a Customer with a flatten relationship to products containing 1 product
    // Customer ---->> CustomerProduct --> Product
    // ==> filteredRelationships = customerProducts (a to-many)

    // for each relationship we go down the tree
    // It should return the same sub tree
    val subEOLists: List[List[Future[EOContaining]]] = filteredRelationships.map(relationship => {
      val eoValueOpt = EOValue.valueForKey(rootContaining, relationship.name)
      eoValueOpt match {
        case Some(eovalue) =>
          eovalue match {
            case ObjectValue(eoPk: EOPk) =>
              traverseDestinationEOWithPk2(eoPk, relationship.destinationEntityName, eosByENameByPk)
            case ObjectsValue(eoPks: List[EOPk]) =>
              traverseDestinationEOsWithPks2(eoPks, relationship.destinationEntityName, eosByENameByPk)
            case _ => List.empty[Future[EOContaining]]
          }
        case None => List.empty[Future[EOContaining]]
      }
    })
    val subEOs: List[Future[EOContaining]] = subEOLists.flatten
    //Logger.debug("traverseTreeToOne | subEOs from children " + subEOs)

    // Update all to-one to children pk
    // return a Tuple with old EOpk as key an update EO as value
    val toOneUpdateOpts = filteredRelationships.map(relationship => {
      if (relationship.isToMany) {
        None
      } else {
        val tupleEOContainingOpt = updateDestinationEO(rootContaining, relationship)
        tupleEOContainingOpt match {
          case Some((eopk, eoContaining)) =>
            val newEOPkOpt = EOValue.pkFromValues(eomodel, eoContaining)
            newEOPkOpt match {
              case Some(newEOPk) =>
                Some(ToOneUpdate(relationship, eopk, ObjectValue(newEOPk), eoContaining))
              case None => None
            }
          case None => None
        }
      }
    })
    val toOneUpdates = toOneUpdateOpts.flatten

    // 1) Update root
    val updateRootMap = toOneUpdates.map(toOneUpdate => {
      (toOneUpdate.relationship.name, toOneUpdate.eovalue)
    }).toMap

    val updatedRoot: EOContaining = EOValue.takeValuesForKeys(rootContaining, updateRootMap)

    // 2) Update cache
    val cacheUpdate: Map[String, Map[EOPk, EOContaining]] = toOneUpdates.map(toOneUpdate => {
      val relationship = toOneUpdate.relationship
      val entityName = relationship.destinationEntityName
      val subMap = Map(toOneUpdate.oldEOPk -> toOneUpdate.eo)
      (entityName, subMap)
    }).toMap

    //Logger.debug("traverseTreeToOne | cacheUpdate " + cacheUpdate)

    // children
    val futureOfList: Future[List[EOContaining]] = Future sequence subEOs

    val updateFEOs: Future[List[Future[EOContaining]]] = futureOfList.map(
      peos => {

        val futureOfEOs: List[Future[EOContaining]] = if (peos.size > 0) {
          //Logger.debug("traverseTreeToOne | peos.size > 0 " + peos)
          val peosMap = eosByEntityNameByPk(peos)

          val newEos = updateCacheWith(peosMap, cacheUpdate)
          //Logger.debug("traverseTreeToOne | newEos " + newEos)
          newEos.map(Future(_))
        } else {
          List.empty[Future[EOContaining]]
        }

        if (isFromToOne) {
          val futureOfSavedRoot = saveEO(updatedRoot)
          futureOfSavedRoot :: futureOfEOs
        } else {
          Future(updatedRoot) :: futureOfEOs
        }
      }
    )
    Await result(updateFEOs, 2 seconds)
  }

  def eoFromCache(cache: Map[String, Map[EOPk, EOContaining]], entityName: String, eoPk: EOPk) = {
    if (cache.contains(entityName)) {
      val eoByPk = cache(entityName)
      if (eoByPk.contains(eoPk)) {
        Some(eoByPk(eoPk))
      } else None
    } else None
  }

  def traverseDestinationEOWithPk2(eoPk: EOPk, destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]) = {
    val eoOpt = eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    eoOpt match {
      case Some(eo) => traverseTreeToOne(eo, eosByEntityNameByPk, true)
      case None => List.empty[Future[EOContaining]]
    }

  }

  def traverseDestinationEOsWithPks2(eoPks: List[EOPk], destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]) = {
    val eoOpts = eoPks.map(eoPk => {
      eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    })
    val newEOs = eoOpts.flatten
    newEOs.map(traverseTreeToOne(_, eosByEntityNameByPk, false)).flatten
  }

  // return the exact same tree except that flattten will be generate extra EO for the intermediate entity
  def savingEOTree(eos: List[EOContaining]): List[EOContaining] = {
    val eosMap = eosByEntityNameByPk(eos)
    traverseTree(eos.head, eosMap)
  }

  def eosByEntityNameByPk(eos: List[EOContaining]): Map[String, Map[EOPk, EOContaining]] = {
    val eosByEntityName = eos.groupBy(_.eo.entityName)
    eosByEntityName.map { case (entityName, gr) => {
      (entityName, gr.map(toto => (toto.eo.pk, toto)).toMap)
    }
    }
  }

  // unflatten relationships
  def traverseTree(root: EOContaining, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]): List[EOContaining] = {
    val entityName = root.eo.entityName
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    val relationships = entity.relationships
    //Logger.debug("Update EO | traverse tree | relationships " + relationships)

    // only the relationships of the EO Keys to optimize
    val filteredRelationships = relationships.filter(rel => {
      root.eo.keys.contains(rel.name)
    })
    //Logger.debug("Update EO | traverse tree | filteredRelationships " + filteredRelationships)

    var updatedRoot = root
    // for each relationship we go down the tree
    val subEOLists: List[List[EOContaining]] = filteredRelationships.map(relationship => {
      relationship.definition match {

        // Flatten. All flatten are considered as many-to-many (Is that ok ?)
        case Some(definition) =>
          //Logger.debug("Update EO | traverse tree | found flatten " + definition)

          // we want to consider flattened many-to-many
          // in such case the object around the link table will have non compound pk
          val rootPk = root.eo.pk.pks.head

          // to many to ending eos
          val eoValueOpt = EOValue.valueForKey(root, relationship.name)
          //Logger.debug("Update EO | traverse tree | valueForKey " + eoValueOpt)

          eoValueOpt match {
            case Some(ObjectsValue(eoPks: List[EOPk])) =>
              val keyPathFirstKey = EORelationship.keyPathFirstKey(definition)
              //Logger.debug("Update EO | traverse tree | keyPathFirstKey " + keyPathFirstKey)

              val relationshipToLinkOpt = EORelationship.relationshipNamed(relationships, keyPathFirstKey)
              //Logger.debug("Update EO | traverse tree | relationshipToLinkOpt " + relationshipToLinkOpt)
              relationshipToLinkOpt match {

                case Some(linkRelationship) =>
                  val linkEntity = EOModelUtils.entityNamed(eomodel, linkRelationship.destinationEntityName).get

                  // Iterate on pks
                  val newEOs: List[Option[EOContaining]] = eoPks.map(eoPk => {
                    val isRootFirst = linkEntity.relationships.head.destinationEntityName.equals(entityName)
                    //Logger.debug("Update EO | traverse tree | isRootFirst " + isRootFirst)

                    val destinationPk = eoPk.pks.head
                    val linkPks = if (isRootFirst) List(rootPk, destinationPk) else List(destinationPk, rootPk)
                    //Logger.debug("Update EO | traverse tree | linkPks " + linkPks)

                    val linkToDestinationRelationshipOpt = linkEntity.relationships.find(rel => {
                      !rel.destinationEntityName.equals(entityName)
                    })
                    //Logger.debug("Update EO | traverse tree | linkToDestinationRelationshipOpt " + linkToDestinationRelationshipOpt)
                    linkToDestinationRelationshipOpt match {
                      case Some(linkToDestinationRelationship) =>
                        val newEO = EO(entityName = linkEntity.name, keys = List(linkToDestinationRelationship.name), values = List(ObjectValue(eoPk)), pk = EOPk(linkPks), saved = false)
                        //Logger.debug("Update EO | traverse tree | newEO " + newEO)

                        Some(EOContainer(newEO))
                      case None =>
                        None
                    }
                  })
                  // Source EO should link to new objects
                  val links: List[EOContaining] = newEOs.flatten
                  val keyValues = EO.mapWith(root.eo.keys, root.eo.values)
                  val cleanedKeyValues = keyValues - relationship.name
                  val linkPks = links.map(_.eo.pk)
                  val fixedKeyValues = cleanedKeyValues + (keyPathFirstKey -> ObjectsValue(linkPks))
                  val children: List[EOContaining] = traverseDestinationEOsWithPks(eoPks, relationship.destinationEntityName, eosByEntityNameByPk)
                  updatedRoot = EO.updateEOWithMap(root, fixedKeyValues)
                  val aggregatedEOs: List[EOContaining] = links ::: children
                  //Logger.debug("Update EO | traverse tree | aggregatedEOs " + aggregatedEOs)
                  aggregatedEOs
                case None =>
                  List.empty[EOContaining]
              }
            case _ => List.empty[EOContaining]
          }

        case None =>
          //Logger.debug("Update EO | traverse tree | non flatten " + relationship.name)

          // Nothing to do
          val eoValueOpt = EOValue.valueForKey(root, relationship.name)
          eoValueOpt match {
            case Some(eovalue) =>
              eovalue match {
                case ObjectValue(eoPk: EOPk) =>
                  traverseDestinationEOWithPk(eoPk, relationship.destinationEntityName, eosByEntityNameByPk)
                case ObjectsValue(eoPks: List[EOPk]) =>
                  traverseDestinationEOsWithPks(eoPks, relationship.destinationEntityName, eosByEntityNameByPk)
                case _ => List.empty[EOContaining]
              }
            case None => List.empty[EOContaining]
          }
      }
    })
    updatedRoot :: subEOLists.flatten
  }

  def traverseDestinationEOWithPk(eoPk: EOPk, destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]) = {
    val eoOpt = eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    eoOpt match {
      case Some(eo) => traverseTree(eo, eosByEntityNameByPk)
      case None => List.empty[EOContaining]
    }

  }

  def traverseDestinationEOsWithPks(eoPks: List[EOPk], destinationEntityName: String, eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]]) = {
    val eoOpts = eoPks.map(eoPk => {
      eoWithEntityNameAndPk(eosByEntityNameByPk, destinationEntityName, eoPk)
    })
    val newEOs = eoOpts.flatten
    newEOs.map(traverseTree(_, eosByEntityNameByPk)).flatten
  }

  def eoWithEntityNameAndPk(eosByEntityNameByPk: Map[String, Map[EOPk, EOContaining]], entityName: String, pk: EOPk) = {
    if (eosByEntityNameByPk.contains(entityName)) {
      val eoByPK = eosByEntityNameByPk(entityName)
      if (eoByPK.contains(pk)) {
        Some(eoByPK(pk))
      } else {
        None
      }
    } else {
      None
    }
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
  def updateEO(eos: List[EOContaining]): Future[List[EOContaining]] = {
    val savingEOs = savingEOTree(eos)
    //Logger.debug("updateEO | savingEOs: " + savingEOs)

    val savedEOsForTo1 = savedEOsForToOne(savingEOs)
    //Logger.debug("updateEO | savedEOsForToOne: " + savedEOsForTo1)

    val futureOfList: Future[List[EOContaining]] = Future sequence savedEOsForTo1

    val toto = futureOfList.map(rrs => {
      val savedEOsForToM = savedEOsForToMany(rrs)
      //Logger.debug("updateEO | savedEOsForToM: " + savedEOsForToM)
      val futureOfList2 = Future sequence savedEOsForToM
      futureOfList2
    })
    toto.flatten
  }

  def extractEOsToSave(eo: EO): List[EO] = {
    List()
  }


  // POST (create)
  // call Project.json with: {"descr":"bb","projectNumber":2}
  // {"id":2,"type":"Project","descr":"bb","projectNumber":2,"customer":null}


  def saveEO(eoContaining: EOContaining): Future[EOContaining] = {
    val eo = eoContaining.eo
    if (!eo.saved) {

      //Logger.debug("saveEO | eo not saved " + eo)
      val isNewEO = EOValue.isNewEO(eoContaining)
      //Logger.debug("saveEO | isNewEO " + isNewEO)

      val entityName = eo.entityName
      //Logger.debug("saveEO | entityName " + entityName)

      val baseUrl = d2spaServerBaseUrl + "/" + entityName
      val url = baseUrl + (if (isNewEO) ".json" else {
        val pk = eo.pk.pks.head
        "/" + pk + ".json"
      })

      val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)

      //Logger.debug("saveEO | baseUrl " + url)
      val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
      entityOpt match {
        case Some(entity) =>
          //Logger.debug("saveEO | entity " + entity)

          //val eoDefinedValues = EOValue.definedValues(eo)
          val eoDefinedValues = EOValue.keyValues(eo)

          //Logger.debug("eoDefinedValues " + eo.values)
          val eoValuesOpts = eoDefinedValues map { case (key, valContainer) => {
            val valueOpt = woWsParameterForValue(valContainer, entity, key)
            valueOpt match {
              case Some(value) => Some((key, value))
              case None => None
            }
          }
          }
          val eoValues = eoValuesOpts.flatten.toMap
          //Logger.debug("eoValues " + eoValues)
          val data = Json.toJson(eoValues)

          //Logger.debug("Save EO | Upate WS:  " + url)
          //Logger.debug("Save EO | WS post data: " + data)

          val futureResponse: Future[WSResponse] = if (isNewEO) request.post(data) else request.put(data)
          // Work with the response: The goal is to update the PK that could have been generated by the server
          // (for single PK only because the multiple pk are reference to other object so we know them already)
          // But we don't set the pk eo attribute yet because the client needs to find it's chicks
          futureResponse.map { response =>
            try {
              val savedEO: EOContaining = if (isNewEO) {
                val pkAttributeNames = entity.pkAttributeNames
                //Logger.debug("New EO | pkAttributeNames: " + pkAttributeNames)
                pkAttributeNames.size match {
                  case 1 =>
                    //Logger.debug("New EO | size = 1")
                    val pkAttributeName = pkAttributeNames.head
                    val resultBody = response.json
                    //Logger.debug("New EO | resultBody: " + resultBody)
                    val jsObj = resultBody.asInstanceOf[JsObject]

                    val pkValue = jsObj.value("id")
                    pkValue match {
                      case JsNumber(pkNumber) =>
                        // We don't set the pk of the EO here because the client has to identify it. It's going to be its responsibility
                        // TODO this contradicts the fact that pk should not be in the value in order to test if it is empty
                        val pkIntValue = pkNumber.intValue()
                        //Logger.debug("New EO | pkIntValue: " + pkIntValue)
                        val updatedEO = EOValue.takeValueForKey(eoContaining, EOValue.intV(pkIntValue), pkAttributeName)
                        //Logger.debug("New EO | updatedEO: " + updatedEO)
                        updatedEO
                      case _ => eoContaining // should never occur but let's have it as a safety nest
                    }

                  case _ =>
                    eoContaining
                }

              } else {
                val resultBody = response.json
                val array = resultBody.asInstanceOf[JsObject]
                //println("updateEO Saved EO " + eo)
                eoContaining
              }
              val eoeo = savedEO.eo.copy(saved = true)
              EOContaining.updateEOContainingEO(savedEO, eoeo)
            } catch {
              case parseException: JsonParseException => {
                println("updateEO | JsonParseException " + parseException)
                handleException(response.body, eoContaining)
              }
              case t: Throwable => {
                println("updateEO | Throwable " + t)
                handleException(t.getMessage(), eoContaining)
              }
            }
          }
        case None =>
          Future(eoContaining)
      }

    } else Future(eoContaining)

  }


  // DELETE
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/Project/2.json
  // => 2

  // Return the EO with everything and may be a validationError
  def deleteEO(eoContaining: EOContaining): Future[EOContaining] = {
    val eo = eoContaining.eo
    val pk = eo.pk.pks.head
    val url = d2spaServerBaseUrl + "/" + eo.entityName + "/" + pk + ".json"
    //Logger.debug("Delete EO: " + eo)
    //Logger.debug("Delete WS: " + url)

    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.delete()
    futureResponse.map { response =>
      try {
        val resultBody = response.json
        eoContaining
      } catch {
        case parseException: JsonParseException => {
          handleException(response.body, eoContaining)
        }
        case t: Throwable => {
          handleException(t.getMessage(), eoContaining)
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
  def woWsParameterForValue(value: EOValue, entity: EOEntity, key: String): Option[JsValue] = {
    value match {
      case StringValue(stringV) => Some(JsString(stringV))

      case IntValue(intV) => Some(JsNumber(intV))

      case ObjectValue(eo) => {
        val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, key)
        destinationEntityOpt match {
          case Some(destinationEntity) =>
            val entityName = destinationEntity.name
            val pk = eo.pks.head
            Some(JsObject(Seq("id" -> JsNumber(pk), "type" -> JsString(entityName))))
          case None => None
        }

      }
      case EmptyValue => Some(JsNull)
      case _ => None
    }

  }


  def handleException(error: String, eoContaining: EOContaining): EOContaining = {
    Logger.debug("error " + error + " with eo " + eoContaining)

    val ueo = eoContaining.eo.copy(validationError = Some(error))
    EOContaining.updateEOContainingEO(eoContaining, ueo)
  }

  var eomodel: EOModel = null

  override def preStart {
    println("Menus Actors: preStart")
    eomodelActor ! GetEOModel(None, self)
  }
  def customerFromCustomerEO(cust: EOContaining) = {
    val trigram = EOValue.stringValueForKey(cust,"customerTrigram")
    val name = EOValue.stringValueForKey(cust,"preferedName")

    val dynamicsAccountIDEOValue = EOValue.valueForKey(cust,"dynamicsAccountID")
    val dynamicsAccountID = dynamicsAccountIDEOValue match {
      case Some(StringValue(s)) => Some(s)
      case Some(EmptyValue) => None
      case _ => None
    }
    val id = EOValue.intValueForKey(cust, "id")
    //println("cust " + cust)


    Customer(trigram, id, name, "Operator", dynamicsAccountID, "Switzerland", "EMEA")
  }


  def countryFromCountryEO(eo: EOContaining) = {
    val flagImage = EOValue.stringValueForKey(eo,"flagPath")
    val isoCode = EOValue.stringValueForKey(eo,"isoCode")
    val name = EOValue.stringValueForKey(eo,"name")
    val id = EOValue.intValueForKey(eo, "id")


    Country(id, flagImage, isoCode, name, "EMEA")
  }


  // http://localhost:9000/#admin

  def receive = LoggingReceive {


    case ImportCountries(requester) =>
      println("EORepoActor receive ImportCountries request")
      val fs = EOFetchAll("Country")
      val missingKeys = List("comments", "flagPath" , "isoCode", "name", "region.isoCode")
      search(fs, missingKeys, true).map(eos => {

        val oneCountry = eos.head
        println("One country " + oneCountry)
        search(fs, missingKeys, false).map(lagomEos => {
          val existingTrigrams = lagomEos.map(c => {
            EOValue.stringValueForKey(c,"isoCode")
          }).toSet
          val notYetMigratedObjs = eos.filter( c => {
            val trigram = EOValue.stringValueForKey(c,"isoCode")
            !existingTrigrams.contains(trigram)
          })
          context.system.scheduler.scheduleOnce(100 milliseconds, self, ImportCountry(self, notYetMigratedObjs))

        })


      })
    case ImportCountry(requester, restOfEOs) =>
      if (restOfEOs.size > 0) {
        val eo = restOfEOs.head
        val rest = restOfEOs.tail


        val country = countryFromCountryEO(eo)
        val data = write(country)
        val request: WSRequest = ws.url("http://localhost:9000/api/country").withRequestTimeout(10000.millis)
        val futureResponse: Future[WSResponse] = request.post(data)
        futureResponse.map(r => {
          r.status match {
            case 200 =>
              println("Success creating country " + country)

            case _ =>
              println("Error creating country " + country)
          }

        })

        context.system.scheduler.scheduleOnce(50 milliseconds, self, ImportCountry(self, rest))
      }


    case ImportCustomers(requester) =>
      println("EORepoActor receive ImportCustomers request")
      val missingKeys = List("customerTrigram", "preferedName" , "isChipsetVendorNumber", "dynamicsAccountID", "headCountry.isoCode", "region.isoCode")

      val entityName = "Customer"
      val url = woUrlWith(entityName, None, missingKeys)
      println("EORepoActor | ImportCustomers | url " + url)

      val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
      val futureResponse: Future[WSResponse] = request.get()
      futureResponse.map { response =>
        println("EORepoActor | ImportCustomers | response " + response)

        val resultBody = response.json
        //println("resultBody " + resultBody)

        val array = resultBody.asInstanceOf[JsArray]
        val istCustomers = array.value.map(item => {
          println("EORepoActor | ImportCustomers | item " + item)

          val json = item.asInstanceOf[JsObject]
          val trigramOpt = (json \ "customerTrigram" ).asOpt[String]

          //println("EORepoActor | ImportCustomers | trigramOpt " + trigramOpt)

          val customerIDOpt = (json \ "id" ).asOpt[Long]

          //println("EORepoActor | ImportCustomers | customerIDOpt " + customerIDOpt)
          val nameOpt = (json \ "preferedName" ).asOpt[String]
          //println("EORepoActor | ImportCustomers | nameOpt " + nameOpt)
          val dynamicsAccountIDOpt = (json \ "dynamicsAccountID" ).asOpt[String]
          //println("EORepoActor | ImportCustomers | dynamicsAccountIDOpt " + dynamicsAccountIDOpt)
          val dynamicsAccountID = dynamicsAccountIDOpt match {
             case Some(id) => id
             case _ => null
           }

          val countryIsoCodeOpt = (json \ "headCountry" \ "isoCode").asOpt[String]
          //println("EORepoActor | ImportCustomers | countryIsoCodeOpt " + countryIsoCodeOpt)
          val regionIsoCodeOpt = (json \ "region" \ "isoCode").asOpt[String]
          //println("EORepoActor | ImportCustomers | regionIsoCodeOpt " + regionIsoCodeOpt)

          val countryIsoCode = countryIsoCodeOpt match {
            case Some(id) => id
            case _ => "US"
          }

          val regionIsoCode = regionIsoCodeOpt match {
            case Some(id) => id
            case _ => "EMEA"
          }

          CustomerForCreation(trigramOpt.get, customerIDOpt.get, nameOpt.get, "Operator", dynamicsAccountID, countryIsoCode, regionIsoCode)
        })
        Logger.debug("Import Customers: IST customers " + istCustomers.size)

        val fs = EOFetchAll(entityName,List())
        search(fs, missingKeys, false).map(lagomEos => {
          Logger.debug("Import Customers: Lagom customers " + lagomEos.size)


          val existingTrigrams = lagomEos.map(c => {
            EOValue.stringValueForKey(c,"customerTrigram")
          }).toSet
          val notYetMigratedObjs = istCustomers.filter( istCustomer => {
            val trigram = istCustomer.trigram
            !existingTrigrams.contains(trigram)
          }).toList
          context.system.scheduler.scheduleOnce(100 milliseconds, self, ImportCustomer(self, notYetMigratedObjs))

        })
      }


    case ImportCustomer(requester, customers) =>
      if (customers.size > 0) {
        val customer = customers.head
        val rest = customers.tail
        //val rest = List()

        val data = write(customer)
        println("data " + data)
        val request: WSRequest = ws.url("http://localhost:9000/api/customer").withRequestTimeout(10000.millis)
        val futureResponse: Future[WSResponse] = request.post(data)
        futureResponse.map(r => {
          r.status match {
            case 200 =>
              println("Success creating customer " + customer)

            case _ =>
              println("Error creating customer " + customer)
          }

        })

        context.system.scheduler.scheduleOnce(50 milliseconds, self, ImportCustomer(self, rest))

      }

    //case class CustomerCreated(trigram: String, name: String, customerType: String, dynamicsAccountID: String, headCountry: String, region: String) extends CustomerEvent

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
      println("Get Hydrate")
      hydration.drySubstrate match {
        case DrySubstrate(_, Some(eoFault), _) =>
          val missingKeys = RulesUtilities.missingKeysWith(hydration.wateringScope, ruleResults)
          //println("Hydrate missingKeys " + missingKeys)
          //println("Hydrate hydration.wateringScope " + hydration.wateringScope)

          hydrateEOs(eoFault.entityName, List(eoFault.pk), missingKeys.toSet).map(rrs => {
            //println("Hydrate gives " + rrs.size)
            //println("Hydrate send to " + requester)
            requester ! CompletedEOs(d2wContextOpt, hydration, rrs, ruleResults)
          }
          )
        case DrySubstrate(_, _, Some(fs)) =>
          val missingKeys = RulesUtilities.missingKeysWith(hydration.wateringScope, ruleResults)
          //println("Hydrate fs : " + fs)
          //println("Hydrate with missingKeys " + missingKeys)
          search(fs, missingKeys, false).map(rrs =>
            requester ! CompletedEOs(d2wContextOpt, hydration, rrs, ruleResults)
          )
        case DrySubstrate(Some(eosFault), _, _) =>
          val missingKeys = RulesUtilities.missingKeysWith(hydration.wateringScope, ruleResults)
          println("Hydrate missingKeys " + missingKeys)
          println("Hydrate hydration.wateringScope " + hydration.wateringScope)

          hydrateEOs(eosFault.entityName, eosFault.pks, missingKeys.toSet).map(rrs => {
            println("Hydrate gives " + rrs.size)
            println("Hydrate send to " + requester)
            requester ! CompletedEOs(d2wContextOpt, hydration, rrs, ruleResults)
          }
          )

      }


    case NewEO(d2wContext: D2WContext, eos: List[EO], ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      Logger.debug("Get NewEO " + eos)
      updateEO(eos).map(rrs =>
        requester ! SavingResponse(d2wContext, rrs, ruleResults)
      )
    case UpdateEO(d2wContext: D2WContext, eos: List[EO], ruleResults: Option[List[RuleResult]], requester: ActorRef) =>
      Logger.debug("Get UpdateEO")
      updateEO(eos).map(rrs =>
        requester ! SavingResponse(d2wContext, rrs, ruleResults)
      )


    case DeleteEO(eo: EO, requester: ActorRef) =>
      log.debug("Get GetRulesForMetaData")
      deleteEO(eo).map(rrs =>
        requester ! DeletingResponse(rrs)
      )

  }


}

