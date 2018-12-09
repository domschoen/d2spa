package models

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import javax.inject.Inject
import models.EOModelActor.{EOModelResponse, GetEOModel}
import models.EORepoActor.{DeletingResponse, FetchedObjects, FetchedObjectsForList, SavingResponse}
import models.MenusActor.{GetMenus, MenusResponse}
import models.NodeActor.SetItUp
import models.RulesActor.{GetRule, GetRulesForMetaData, RuleResultsResponse}
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
    case EOModelResponse(eomodel) =>
      println("Receive EOModelResponse ---> sending FetchedEOModel")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedEOModel(eomodel))
      out ! FetchedEOModel(eomodel)

    case MenusResponse(menus) =>
      println("Receive MenusResponse ---> sending FetchedMenus")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedMenus(menus))
      out ! FetchedMenus(menus)

    case RuleResultsResponse(ruleResults) =>
      println("Receive RuleResultsResponse ---> sending RuleResults")
      //context.system.scheduler.scheduleOnce(5 second, out, RuleResults(ruleResults))
      out ! RuleResults(ruleResults)

    case FetchedObjects(eos) =>
      println("Receive FetchedObjects ---> sending FetchedObjectsMsgOut")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedObjectsMsgOut(eos))
      out ! FetchedObjectsMsgOut(eos)


    case FetchedObjectsForList(fs, eos) =>
      println("Receive FetchedObjectsForList ---> sending FetchedObjectsForListMsgOut")
      //context.system.scheduler.scheduleOnce(5 second, out, FetchedObjectsMsgOut(eos))
      out ! FetchedObjectsForListMsgOut(fs, eos)


    case SavingResponse(eo) =>
      println("Receive SavingResponse ---> sending SavingResponseMsgOut")
      out ! SavingResponseMsgOut(eo)

    case DeletingResponse(eo) =>
      println("Receive DeletingResponse ---> sending DeletingResponseMsgOut")
      out ! DeletingResponseMsgOut(eo)


    case msg: WebSocketMsgIn => msg match {
      case DeleteEOMsgIn(eo) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.DeleteEO(eo, self)

      case NewEO(entityName,eo) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.NewEO(entityName,eo, self)


      case UpdateEO(eo) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.UpdateEO(eo, self)

      case HydrateAll(fs) =>
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.HydrateAll(fs, self)

      case Search(fs) =>
        println("Receive Search")
        context.actorSelection("akka://application/user/node-actor/eoRepo") ! EORepoActor.Search(fs, self)

      case GetDebugConfiguration =>
        println("Receive GetDebugConfiguration ---> sending DebugConfMsg")
        out ! DebugConfMsg(showDebugButton)

      case FetchEOModel =>
        println("Receive FetchEOModel")
        context.actorSelection("akka://application/user/node-actor/eomodelFetcher") ! GetEOModel(self)

      case FetchMenus =>
        println("Receive FetchMenus")
        context.actorSelection("akka://application/user/node-actor/menusFetcher") ! GetMenus(self)

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
