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
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import d2spa.shared._
import models.EOModelActor.{EOModelResponse, GetEOModel}
import models.MenusActor.{GetMenus, MenusResponse}

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global

case class EntityItem(
                       name: String,
                       pkAttributeName: String
                     )

case class MenuItem(
                     id: Int,
                     `type`: String,
                     title: String,
                     entity: EntityItem,
                     parent: Option[MenuItem]
                   )

object MenusActor {
  def props(eomodelActor: ActorRef, ws: WSClient): Props = Props(new MenusActor(eomodelActor, ws))

  case class MenusResponse(menus: Menus, d2wContext: D2WContextFullFledged)

  case class GetMenus(d2wContext: D2WContextFullFledged, requester: ActorRef)

}

class MenusActor(eomodelActor: ActorRef, ws: WSClient) extends Actor with ActorLogging {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")


  implicit lazy val entityItemReads: Reads[EntityItem] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "pkAttributeName").read[String]
    ) (EntityItem.apply _)



  implicit lazy val menuItemReads: Reads[MenuItem] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "type").read[String] and
      (JsPath \ "title").read[String] and
      ((JsPath \ "entity").read[EntityItem] orElse (Reads.pure(null))) and
      (JsPath \ "parent").lazyReadNullable(menuItemReads)
    ) (MenuItem.apply _)

  def getMenus(): Future[Menus] = {
    Logger.debug("get Menus")

    val fetchedEOModel = eomodel
    val url = d2spaServerBaseUrl + "/Menu.json";
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
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
          case e: JsError => Logger.error("Errors: " + JsError.toJson(e).toString())
        }
      }
      val children = menus.filter(_.parent.isDefined)
      val childrenByParent = children.groupBy(_.parent.get)

      val mainMenus = childrenByParent.map {
        case (mm, cs) =>

          val childMenus = cs.map(cm => {
            Logger.debug("LOOK for " + cm.entity.name + " into eomodel " + fetchedEOModel)

            val entity = EOModelUtils.entityNamed(fetchedEOModel, cm.entity.name).get
            Menu(cm.id, cm.title, entity)
          })
          MainMenu(mm.id, mm.title, childMenus)
      }
      if (mainMenus.isEmpty) {
        // TOTO Menus containing D2WContext is not a good choice because better to have
        // None D2WContext if no menus (at least, D2WContext should be an option instead of returning
        // D2WContext(null,null,null)

        Menus(List()) //, showDebugButton)
      } else {
        val firstChildEntity = mainMenus.head.children.head.entity
        Menus(mainMenus.toList) //, showDebugButton)
      }
    }
  }

  var eomodel: EOModel = null

  override def preStart {
    println("Menus Actors: preStart")
    eomodelActor ! GetEOModel(None, self)
  }

  def receive = LoggingReceive {
    case EOModelResponse(model, d2wContext) =>
      eomodel = model

    case GetMenus(d2wContext, requester) => {
      log.debug("Get Menus")
      getMenus().map({
        requester ! MenusResponse(_, d2wContext)

      })
    }
  }


}

