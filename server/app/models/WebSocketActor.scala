package models

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import javax.inject.Inject
import models.EOModelActor.{EOModelResponse, GetEOModel}
import models.MenusActor.{GetMenus, MenusResponse}
import models.NodeActor.SetItUp
import models.RulesActor.{GetRule, GetRulesForMetaData, MetaDataResponse, HydrateEOsForDisplayPropertyKeys, RuleResultsResponse}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.ws.WSClient

import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global

class WebSocketActor (out: ActorRef, nodeActor: ActorRef) extends Actor with ActorLogging {
  val config = ConfigFactory.load()
  val showDebugButton = if (config.getIsNull("d2spa.showDebugButton")) true else config.getBoolean("d2spa.showDebugButton")


  /*val eomodelActor = context.actorOf(EOModelActor.props(), "eomodelFetcher")
val menusActor = context.actorOf(MenusActor.props(eomodelActor), "menusFetcher")
val rulesActor = context.actorOf(RulesActor.props(eomodelActor), "rulesFetcher")
val eoRepoActor = context.actorOf(EORepoActor.props(eomodelActor), "eoRepo")*/

  override def preStart: Unit = {
    nodeActor ! SetItUp
  }


  def receive = {
    case EOModelResponse(eomodel, d2wContextOpt) =>
      println("Receive EOModelResponse ---> sending FetchedEOModel")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedEOModel(eomodel))
      out ! FetchedEOModel(eomodel, d2wContextOpt.get)

    case MenusResponse(menus, d2wContext) =>
      println("Receive MenusResponse ---> sending FetchedMenus")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedMenus(menus))
      out ! FetchedMenus(menus, d2wContext)

    case RuleResultsResponse(ruleResults) =>
      println("Receive RuleResultsResponse ---> sending RuleResults")
      //context.system.scheduler.scheduleOnce(5 second, out, RuleResults(ruleResults))
      out ! RuleResults(ruleResults)

    case MetaDataResponse(d2wContext: D2WContextFullFledged, ruleResults) =>
      println("Receive RuleResultsResponse ---> sending RuleResults")
      out ! MetaDataResponseMsg(d2wContext, Some(ruleResults))

    case EORepoActor.CompletedEO(d2wContext, eo, ruleResultsOpt) =>
      println("Receive CompletedEO ---> sending FetchedObjectsMsgOut")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedObjectsMsgOut(eos))
      out ! CompletedEOMsgOut(d2wContext, eo, ruleResultsOpt)

    case EORepoActor.FetchedObjects(entityName, eos, ruleResultsOpt) =>
      println("Receive FetchedObjects ---> sending FetchedObjectsMsgOut")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedObjectsMsgOut(eos))
      out ! FetchedObjectsMsgOut(entityName, eos, ruleResultsOpt)


    case EORepoActor.FetchedObjectsForList(fs, eos, ruleResultsOpt) =>
      println("Receive FetchedObjectsForList ---> sending FetchedObjectsForListMsgOut")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedObjectsMsgOut(eos))
      out ! FetchedObjectsForListMsgOut(fs, eos, ruleResultsOpt)

    case EORepoActor.SavingResponse(d2wContext: D2WContextFullFledged, eo: EO, ruleResults: Option[List[RuleResult]]) =>
      println("Receive SavingResponse ---> sending SavingResponseMsgOut")
      out ! SavingResponseMsgOut(d2wContext, eo, ruleResults)

    case EORepoActor.DeletingResponse(eo) =>
      println("Receive DeletingResponse ---> sending DeletingResponseMsgOut")
      out ! DeletingResponseMsgOut(eo)


    case msg: WebSocketMsgIn => msg match {
      case DeleteEOMsgIn(eo) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.DeleteEO(eo, self)

      case NewEO(d2wContext,eo, isMetaDataFetched) =>
        val entityName = d2wContext.entityName.get
        if (isMetaDataFetched) {
          println("Receive NewEO ---> sending EORepoActor NewEO")
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.NewEO(d2wContext, eo, None, self)
        } else {
          println("Receive NewEO ---> sending RulesActor GetMetaDataForNewEO")
          context.actorSelection("akka://application/user/node-actor/rulesFetcher") !
            RulesActor.GetMetaDataForNewEO(d2wContext, eo, self)
        }

      case UpdateEO(eo) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.UpdateEO(eo, self)

      case HydrateAll(fs) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.HydrateAll(fs, self)

      case CompleteEO(d2wContext, eoFault, missingKeys) =>
        val entityName = d2wContext.entityName.get
        if (missingKeys.isEmpty) {
          println("Receive CompleteEO ---> sending RulesActor GetMetaDataForEOCompletion")
          context.actorSelection("akka://application/user/node-actor/rulesFetcher") !
            RulesActor.GetMetaDataForEOCompletion(d2wContext, eoFault, self)
        } else {
          println("Receive CompleteEO ---> sending EORepoActor CompleteEO")
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.CompleteEO(d2wContext, eoFault, missingKeys, None, self) //: Future[Seq[EO]]
        }



      case HydrateEOs(d2wContext, pks: Seq[EOPk], missingKeys) =>
        val entityName = d2wContext.entityName.get
        if (missingKeys.isEmpty) {
          context.actorSelection("akka://application/user/node-actor/rulesFetcher") !
            HydrateEOsForDisplayPropertyKeys(d2wContext, pks: Seq[EOPk], self)
        } else {
          context.actorSelection("akka://application/user/node-actor/eoRepo") !
            EORepoActor.HydrateEOs(entityName, pks, missingKeys, None, self) //: Future[Seq[EO]]
        }

      case Search(fs, isMetaDataFetched) =>
        if (isMetaDataFetched) {
          println("Receive Search ---> sending EORepoActor Search")
          context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.Search(fs, None, self)
        } else {
          println("Receive NewEO ---> sending EORepoActor GetMetaDataForSearch")
          context.actorSelection("akka://application/user/node-actor/rulesFetcher") !
            RulesActor.GetMetaDataForSearch(fs, self)

        }

      case GetDebugConfiguration(d2wContext) =>
        println("Receive GetDebugConfiguration ---> sending DebugConfMsg")
        out ! DebugConfMsg(showDebugButton,d2wContext)

      case FetchEOModel(d2wContext) =>
        println("Receive FetchEOModel")
        context.actorSelection("akka://application/user/node-actor/eomodelFetcher") ! GetEOModel(Some(d2wContext), self)

      case FetchMenus(d2wContext) =>
        println("Receive FetchMenus")
        context.actorSelection("akka://application/user/node-actor/menusFetcher") ! GetMenus(d2wContext, self)

      case GetMetaData(d2wContext) =>
        println("Receive GetMetaData")
        context.actorSelection("akka://application/user/node-actor/rulesFetcher") ! GetRulesForMetaData(d2wContext,self)

      case RuleToFire(d2wContext: FiringD2WContext, key: String) =>
        println("Receive RuleToFire")
        context.actorSelection("akka://application/user/node-actor/rulesFetcher")  ! GetRule(d2wContext, key, self)

    }
  }
}

object WebSocketActor {
  def props(out: ActorRef, nodeActor: ActorRef): Props = Props(new WebSocketActor(out, nodeActor))
}
