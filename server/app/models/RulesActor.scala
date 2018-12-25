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

  case class GetRule(d2wContext: FiringD2WContext, key: String, requester: ActorRef)

  case class GetRulesForMetaData(d2wContext: D2WContextFullFledged, requester: ActorRef)

  case class HydrateEOsForDisplayPropertyKeys(d2wContext: D2WContextFullFledged, pks: Seq[EOPk], requester: ActorRef)

  case class GetMetaDataForEOCompletion(d2wContext: D2WContextFullFledged, eoFault: EOFault, requester: ActorRef)
}

class RulesActor (eomodelActor: ActorRef, ws: WSClient) extends Actor with ActorLogging {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")



  // To create the result, it needs to issue multiple query on the D2W rule system
  // 1) get entity display name for edit, inspect, query, list
  // D2WContext: task=edit, entity=..
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayNameForEntity
  // http://localhost:1666/cgi-bin/WebObjects/D2SPAServer.woa/ra/fireRuleForKey.json?task=edit&entity=Customer&key=displayPropertyKeys
  def getRuleResultsForMetaData(d2wContext: D2WContextFullFledged): Future[List[RuleResult]] = {
    val fetchedEOModel = eomodel

    val fD2WContext = RulesUtilities.convertFullFledgedToFiringD2WContext(d2wContext)
    val entityDisplayNameFuture = fireRuleFuture(fD2WContext, RuleKeys.displayNameForEntity)
    val displayPropertyKeysFuture = fireRuleFuture(fD2WContext, RuleKeys.displayPropertyKeys)

    val result = for {
      r1 <- entityDisplayNameFuture
      r2 <- displayPropertyKeysFuture
    } yield {
      val entityDisplayName = fromRuleResponseToKeyAndString(r1)
      val displayPropertyKeys = fromRuleResponseToKeyAndArray(r2)
      val entityName = d2wContext.entityName.get
      Logger.debug("LOOK for " + entityName + " into eomodel " + fetchedEOModel)

      val propertiesRuleResults =  propertyMetaInfosForTask(fD2WContext, displayPropertyKeys._2).flatten

      List(
        RuleResult(d2wContext, entityDisplayName._1, RuleValue(Some(entityDisplayName._2))),
        RuleResult(d2wContext, displayPropertyKeys._1, RuleValue(stringsV = displayPropertyKeys._2.toList))
      ) ::: propertiesRuleResults
    }
    result


  }



  val fireRuleArguments = List("entity", "task", "propertyKey", "pageConfiguration", "key")

  def fireRuleFuture(rhs: FiringD2WContext, key: String): Future[WSResponse] = {
    //Logger.debug("Fire Rule for key " + key + " rhs:" + rhs)
    val url = d2spaServerBaseUrl + "/fireRuleForKey.json";
    //val entityName = entity.map(_.name)
    val fireRuleValues = List(rhs.entityName, rhs.task, rhs.propertyKey, rhs.pageConfiguration.value.right.get, Some(key))
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

  private def lift[T](futures: Seq[Future[T]]) =
    futures.map(_.map {
      Success(_)
    }.recover { case t => Failure(t) })


  def waitAll[T](futures: Seq[Future[T]]) =
    Future.sequence(lift(futures)) // having neutralized exception completions through the lifting, .sequence can now be used



  def fireRule(rhs: FiringD2WContext, key: String): Future[RuleResult] = {
    Logger.debug("Fire rule for key " + key + " and d2wContext: " + rhs)
    val f = fireRuleFuture(rhs, key)
    f.map(ruleResultWithResponse(rhs, _))
  }



  def ruleResultWithResponse(rhs: FiringD2WContext, response: WSResponse) = {
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

    val fullFledged = RulesUtilities.convertD2WContextToFullFledged(rhs)

    val result = RuleResult(fullFledged, key, ruleValue)
    Logger.debug("Result " + result)
    result
    //RuleResult(RuleUtils.convertD2WContextToFullFledged(rhs), key, ruleValue.stringV.get)
  }


  // Fetch the following information for each property
  // - displayNameForProperty
  // - componentName
  // - attributeType
  def propertyMetaInfosForTask(d2wContext: FiringD2WContext, displayPropertyKeys: Seq[String]) = {
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
        val fullFledged = RulesUtilities.convertD2WContextToFullFledged(rhs)

          List(
            RuleResult(fullFledged, attributeType._1, RuleValue(Some(attributeType._2))),
            RuleResult(fullFledged, propertyDisplayName._1, RuleValue(Some(propertyDisplayName._2))),
            RuleResult(fullFledged, propertyComponentName._1, RuleValue(Some(propertyComponentName._2)))
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
    eomodelActor ! GetEOModel(self)
  }

  def receive = LoggingReceive {
    case EOModelResponse(model) =>
      eomodel = model

     // for a D2WContext, give ruleResults for
     // - displayNameForEntity
     // - displayPropertyKeys
     // and for each displayPropertyKeys:
     // - displayNameForProperty
     // - componentName
     // - attributeType
    case GetRulesForMetaData(d2wContext, requester) =>
      println("Get GetRulesForMetaData")

      getRuleResultsForMetaData(d2wContext).map(rrs =>
        requester ! RuleResultsResponse(rrs)
      )

    case GetRule(d2wContext, key, requester) => {
      println("Get Rule")
      fireRule(d2wContext, key).map(rr =>
        requester ! RuleResultsResponse(List(rr))
      )
    }
    case HydrateEOsForDisplayPropertyKeys(d2wContext, pks: Seq[EOPk], requester) =>
      val fD2WContext = RulesUtilities.convertFullFledgedToFiringD2WContext(d2wContext)
      fireRule(fD2WContext, "displayPropertyKeys").map(rr => {

          val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(Some(rr))
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.HydrateEOs(fD2WContext.entityName.get, pks, displayPropertyKeys.toSet, Some(List(rr)), self) //: Future[Seq[EO]]
        }
      )
    case GetMetaDataForEOCompletion(d2wContext: D2WContextFullFledged, eoFault: EOFault, requester: ActorRef) =>
      val fD2WContext = RulesUtilities.convertFullFledgedToFiringD2WContext(d2wContext)
      getRuleResultsForMetaData(d2wContext).map(rr => {
        val ruleResult = RulesUtilities.ruleResultForKey(rr, RuleKeys.displayPropertyKeys)
        val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(Some(ruleResult.get))
        context.actorSelection("akka://application/user/node-actor/eoRepo") !
          EORepoActor.CompleteEO(eoFault, displayPropertyKeys.toSet, Some(rr), self) //: Future[Seq[EO]]
      }
      )


  }


}

