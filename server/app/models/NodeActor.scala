package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import com.typesafe.config.ConfigFactory

import scala.xml.Utility
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import javax.inject._
import models.NodeActor.SetItUp
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport




object NodeActor {

  case object SetItUp

}


class NodeActor  @Inject() (configuration: Configuration, ws: WSClient) extends Actor with ActorLogging with InjectedActorSupport {
  val timeout = 10.seconds
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")

  var eomodelActor: Option[ActorRef] = None
  var menusActor: Option[ActorRef] = None
  var rulesActor: Option[ActorRef] = None
  var eoRepoActor: Option[ActorRef] = None

  def receive = LoggingReceive {
    // to the browser
    case SetItUp => {
      println("Set it up")
      if (eomodelActor.isEmpty) {
        eomodelActor = Some(context.actorOf(EOModelActor.props(ws), "eomodelFetcher"))
        println("EOModel actor path " + eomodelActor.get.path)
      }
      if (menusActor.isEmpty) {
        menusActor = Some(context.actorOf(MenusActor.props(eomodelActor.get,ws), "menusFetcher"))
        println("Menu actor path " + menusActor.get.path)
      }
      if (rulesActor.isEmpty) {
        rulesActor = Some(context.actorOf(RulesActor.props(eomodelActor.get, ws), "rulesFetcher"))
        println("Rule actor path " + rulesActor.get.path)
      }
      if (eoRepoActor.isEmpty) {
        eoRepoActor = Some(context.actorOf(EORepoActor.props(eomodelActor.get ,ws), "eoRepo"))
        println("EO Repo actor path " + eoRepoActor.get.path)
      }
    }
  }


}

