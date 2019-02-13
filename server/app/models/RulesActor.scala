package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import d2spa.shared.WebSocketMessages.FetchedEOModel

import scala.xml.Utility
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.ws
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import d2spa.shared._
import javax.inject.Inject
import models.EOModelActor.{EOModelResponse, GetEOModel}
import models.EORepoActor.CompletedEOs
import models.MenusActor.{GetMenus, MenusResponse}
import models.RulesActor._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import play.api.Play.current
import play.api.libs.concurrent.InjectedActorSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}



object RulesActor {
  def props(eomodelActor: ActorRef, ws: WSClient): Props = Props(new RulesActor(eomodelActor, ws))

  case class RuleResultsResponse(ruleResults: List[RuleResult])
  case class RuleRequestResponse(d2wContext: D2WContext, ruleResults: Option[List[RuleResult]])

  case class RulesForSearchResultResponse(fs: EOFetchSpecification, eos: Seq[EO], ruleResults:  Option[List[RuleResult]])
  case class RuleRequestForAppInitResponse(d2wContext: D2WContext, ruleResults: List[RuleResult], eoOpt: Option [EO])

  case class GetRule(d2wContext: D2WContext, key: String, requester: ActorRef)

  case class GetRulesForRequest(ruleRequest: RuleRequest, requester: ActorRef)
  case class GetRulesForAppInit(ruleRequest: RuleRequest, eoOpt: Option [EO], requester: ActorRef)
  case class GetRulesForHydration(d2wContext: Option[D2WContext], ruleRequest: RuleRequest, hydration: Hydration, requester: ActorRef)


  case class HydrateEOsForDisplayPropertyKeys(d2wContext: D2WContext, pks: Seq[EOPk], requester: ActorRef)

  case class GetMetaDataForEOCompletion(d2wContext: D2WContext, eoFault: EOFault, requester: ActorRef)

  case class GetMetaDataForNewEO(d2wContext: D2WContext, eos: List[EO], ruleRequest: RuleRequest, requester: ActorRef)
  case class GetMetaDataForUpdatedEO(d2wContext: D2WContext, eos: List[EO],ruleRequest: RuleRequest, requester: ActorRef)

  case class GetMetaDataForSearch(fs: EOFetchSpecification, requester: ActorRef)

  case class GetRulesForSearchResult(fs: EOFetchSpecification, eos: Seq[EO], ruleRequest: RuleRequest, requester: ActorRef)

}

class RulesActor (eomodelActor: ActorRef, ws: WSClient) extends Actor with ActorLogging {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")






  def unGappedRuleRequest(ruleRequest: RuleRequest): RuleRequest = {
    val d2wContext = ruleRequest.d2wContext

    // Only rules to fire to bridge the gaps
    val fireRulesOpt: List[Option[FireRule]] = ruleRequest.rules.map(r => r match {
      case GappedFireRules(keyToFire, key) =>
        Some(FireRule(keyToFire))
      case _ =>
        None
    })
    val fireRules =  fireRulesOpt.flatten
    if (fireRules.isEmpty) {
      ruleRequest
    } else {
      val fireRulesSet = fireRules.toSet
      val fireRulesList = fireRulesSet.toList
      val fireRuleFutures = fireRulesList.map(fr => {
        fireRuleFuture(d2wContext,fr.key)
      })
      val futureOfList = Future sequence fireRuleFutures
      val ruleFiringWSResponses = Await result(futureOfList, 2 seconds)
      val ruleResults = ruleFiringWSResponses.map( wsresponse => {
        ruleResultWithResponse(d2wContext,wsresponse)
      })

      println("unGappedRuleRequest | ruleResults " + ruleResults)


      // Rules Results available for gaps
      // Re-create the RuleRequest
      val newRules = ruleRequest.rules.map(r => r match {

        case fr: FireRule => fr
        case frs : FireRules => frs
        case gfr: GappedFireRules =>
          val ruleResultOpt = RulesUtilities.ruleResultFromRuleResultsForContextAndKey(ruleResults, d2wContext, gfr.fromRuleKey)
          val propertyKeys = RulesUtilities.ruleListValueWithRuleResult(ruleResultOpt)
          FireRules(propertyKeys,gfr.key)
      })
      ruleRequest.copy(rules = newRules)
    }
  }

  def getRuleResultsForRuleRequest(ruleRequest: RuleRequest): Future[List[RuleResult]] = {
    println("getRuleResultsForRuleRequest | ruleRequest: " + ruleRequest)
    val ruleRequestUG = unGappedRuleRequest(ruleRequest)
    println("getRuleResultsForRuleRequest | ungapped ruleRequest: " + ruleRequestUG)
    val d2wContext = ruleRequestUG.d2wContext
    val fireRuleFutures: List[Future[List[RuleResult]]] = ruleRequestUG.rules.map(fr =>
      fr match {
        case fr: FireRule =>
          val wsresponse = fireRuleFuture(d2wContext, fr.key)
          val toto = wsresponse.map(wsr => {
            List(ruleResultWithResponse(d2wContext, wsr))
          })
          toto

        case frs: FireRules =>
          val propertyKeys = frs.propertyKeys
          val toto: List[Future[List[RuleResult]]] = propertyKeys.map(propertyKey => {
            val propertyD2WContext = d2wContext.copy(propertyKey = Some(propertyKey))
            val wsresponse = fireRuleFuture(propertyD2WContext, frs.key)
            wsresponse.map(wsr => {
              List(ruleResultWithResponse(propertyD2WContext, wsr))
            })
          })
          val unflats: Future[List[List[RuleResult]]] = Future sequence toto
          val titi: Future[List[RuleResult]] = unflats.map(unflat => unflat.flatten)
          titi
          //Future(List.empty[RuleResult])
        //case gfr: GappedFireRules =>

      }
    )
    val futureOfListOfList = Future sequence fireRuleFutures
    futureOfListOfList.map(ll => {
      ll.flatten
    })
  }


  val fireRuleArguments = List("entity", "task", "propertyKey", "pageConfiguration", "key")

  def fireRuleFuture(rhs: D2WContext, key: String): Future[WSResponse] = {
    //Logger.debug("Fire Rule for key " + key + " rhs:" + rhs)
    val url = d2spaServerBaseUrl + "/fireRuleForKey.json";
    //val entityName = entity.map(_.name)
    val fireRuleValues = List(rhs.entityName, rhs.task, rhs.propertyKey, rhs.pageConfiguration, Some(key))
    val nonNullArguments = fireRuleArguments zip fireRuleValues
    val arguments = nonNullArguments.filter(x => !x._2.isEmpty).map(x => (x._1, x._2.get))

    //Logger.debug("Args : " + arguments)
    Logger.debug("Fire Rule with url: " + url)
    Logger.debug("Fire Rule with arguments: " + arguments)

    val request: WSRequest = ws.url(url)
      .withQueryString(arguments.toArray: _*)
      .withRequestTimeout(10000.millis)

    request.get()
  }

  /*private def lift[T](futures: Seq[Future[T]]) =
    futures.map(_.map {
      Success(_)
    }.recover { case t => Failure(t) })*/

/*
  def waitAll[T](futures: Seq[Future[T]]) =
    Future.sequence(lift(futures)) // having neutralized exception completions through the lifting, .sequence can now be used
*/


  def fireRule(rhs: D2WContext, key: String): Future[RuleResult] = {
    Logger.debug("Fire rule for key " + key + " and d2wContext: " + rhs)
    val f = fireRuleFuture(rhs, key)
    f.map(ruleResultWithResponse(rhs, _))
  }



  def ruleResultWithResponse(rhs: D2WContext, response: WSResponse) = {
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
    val ruleValue = jsvalue match {
      case n: play.api.libs.json.JsBoolean =>
        val boolVal = n.value
        val boolString = if (boolVal) "true" else "false"
        RuleValue(Some(boolString))
      case jsArray: play.api.libs.json.JsArray =>
        val (key, value) = fromRuleResponseToKeyAndArray(response)
        RuleValue(stringsV = value.toList)
      case _ =>
        val (key, value) = fromRuleResponseToKeyAndString(response)
        Logger.debug("key  " + key + " value " + value)
        println("key  " + key + " value " + value)
        RuleValue(Some(value))
    }


    val result = RuleResult(rhs, key, ruleValue)
    Logger.debug("Result " + result)
    result
    //RuleResult(RuleUtils.convertD2WContextToFullFledged(rhs), key, ruleValue.stringV.get)
  }


  // Fetch the following information for each property
  // - displayNameForProperty
  // - componentName
  // - attributeType
  def propertyMetaInfosForTask(d2wContext: D2WContext, displayPropertyKeys: Seq[String]) = {
    val propertiesFutures = displayPropertyKeys.map(propertyKey => {
      val rhs = d2wContext.copy(propertyKey = Some(propertyKey))

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

          List(
            RuleResult(rhs, attributeType._1, RuleValue(Some(attributeType._2))),
            RuleResult(rhs, propertyDisplayName._1, RuleValue(Some(propertyDisplayName._2))),
            RuleResult(rhs, propertyComponentName._1, RuleValue(Some(propertyComponentName._2)))
          )

      }
      subResult
    }).toList
    val futureOfList = Future sequence propertiesFutures
    val properties = Await result(futureOfList, 2 seconds)
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





  var eomodel: EOModel = null

  override def preStart {
    println("Rules Actors: preStart")
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
    case GetRulesForRequest(ruleRequest, requester) =>
      println("Get GetRulesForRequest")
      getRuleResultsForRuleRequest(ruleRequest).map(rrs => {
          requester ! RuleRequestResponse(ruleRequest.d2wContext, Some(rrs))
        }
      )

    case GetRulesForAppInit(ruleRequest: RuleRequest, eoOpt, requester: ActorRef) =>
      println("Get GetRulesForAppInit")
      getRuleResultsForRuleRequest(ruleRequest).map(rrs => {
        println("GetRulesForRequest | rule results: " + rrs)
        requester ! RuleRequestForAppInitResponse(ruleRequest.d2wContext, rrs, eoOpt)
      }
      )


    case GetRule(d2wContext, key, requester) => {
      println("Get Rule")
      fireRule(d2wContext, key).map(rr =>
        requester ! RuleResultsResponse(List(rr))
      )
    }
    case GetRulesForHydration(d2wContextOpt, ruleRequest: RuleRequest, hydration: Hydration, requester: ActorRef) =>
      println("GetRulesForHydration " + ruleRequest)
      getRuleResultsForRuleRequest(ruleRequest).map(rrs => {
        println("GetRulesForHydration --> send Hydrate to eorepo" + rrs)

        context.actorSelection("akka://application/user/node-actor/eoRepo") !
          EORepoActor.Hydrate(d2wContextOpt, hydration, Some(rrs), requester)
      }
      )




    case HydrateEOsForDisplayPropertyKeys(d2wContext, pks: Seq[EOPk], requester) =>
      fireRule(d2wContext, "displayPropertyKeys").map(rr => {

          val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(Some(rr))
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.HydrateEOs(d2wContext.entityName.get, pks, displayPropertyKeys.toSet, Some(List(rr)), requester) //: Future[Seq[EO]]
        }
      )

/*    case GetMetaDataForEOCompletion(d2wContext: D2WContext, eoFault: EOFault, requester: ActorRef) =>
      println("Rule Actor Receive GetMetaDataForEOCompletion")
      getRuleResultsForMetaData(d2wContext).map(rr => {
        val ruleResult = RulesUtilities.ruleResultForKey(rr, RuleKeys.displayPropertyKeys)
        val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(Some(ruleResult.get))
        val isNewEO = EOValue.isNew(eoFault.pk)

        if (isNewEO) {
          requester ! CompletedEO(d2wContext, EO(entityName = eoFault.entityName, pk = eoFault.pk), Some(rr))
        } else {
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.CompleteEO(d2wContext, eoFault, displayPropertyKeys.toSet, Some(rr), requester) //: Future[Seq[EO]]
        }
      }
      )*/


    case GetMetaDataForNewEO(d2wContext: D2WContext, eos: List[EO], ruleRequest, requester: ActorRef) =>
      println("Rule Actor Receive GetMetaDataForNewEO")
      getRuleResultsForRuleRequest(ruleRequest).map(rrs =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") !
          EORepoActor.NewEO(d2wContext, eos, Some(rrs), requester) //: Future[Seq[EO]]
      )


    case GetMetaDataForUpdatedEO(d2wContext: D2WContext, eos: List[EO], ruleRequest, requester: ActorRef) =>
      println("Rule Actor Receive GetMetaDataForUpdatedEO")
      getRuleResultsForRuleRequest(ruleRequest).map(rrs =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") !
          EORepoActor.UpdateEO(d2wContext, eos, Some(rrs), requester)
      )




    case GetRulesForSearchResult(fs: EOFetchSpecification, eos, ruleRequest, requester: ActorRef) =>
      println("Rule Actor Receive GetRulesForSearchResult")
      getRuleResultsForRuleRequest(ruleRequest).map(rrs => {
          requester ! RulesForSearchResultResponse(fs, eos, Some(rrs))
      }
      )


  }


}

