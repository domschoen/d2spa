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
import d2spa.shared.{D2WContext, EOEntity, EOModel, EORelationship,EOJoin}
import models.EOModelActor.{EOModelResponse, GetEOModel}

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import javax.inject._
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.ahc._
import play.shaded.ahc.org.asynchttpclient.AsyncHttpClient


case class FetchedEOEntity(
                            name: String,
                            primaryKeyAttributeNames: Seq[String],
                            attributes: Seq[FetchedEOAttribute] = Seq(),
                            relationships: Seq[FetchedEORelationship] = Seq()
                          )

case class FetchedEORelationship(joins: Seq[FetchedEOJoin] = Seq(),
                                 name: String,
                                 definition: Option[String],
                                 isToMany: Boolean,
                                 destinationEntityName: String)

case class FetchedEOAttribute(`type`: String, name: String)

case class FetchedEOJoin(sourceAttribute: FetchedEOAttribute, destinationAttribute: FetchedEOAttribute)


object EOModelActor {
  def props(ws: WSClient): Props = Props(new EOModelActor(ws))

  case class EOModelResponse(eomodel: EOModel, d2wContext: Option[D2WContext])
  case class GetEOModel(d2wContext: Option[D2WContext], requester: ActorRef)

}


class EOModelActor (ws: WSClient) extends Actor with ActorLogging  {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")


  override def preStart {
    println("EOModelActor Actors: preStart: self : " + self)
  }


  implicit lazy val fetchedEOEntityReads: Reads[FetchedEOEntity] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "primaryKeyAttributeNames").read[Seq[String]] and
      (
        (JsPath \ "attributes").lazyRead(Reads.seq(fetchedEOAttributeReads)) or
          Reads.pure(Seq.empty[FetchedEOAttribute])
        ) and
      (
        (JsPath \ "relationships").lazyRead(Reads.seq(fetchedEORelationshipReads)) or
          Reads.pure(Seq.empty[FetchedEORelationship])
        )
    ) (FetchedEOEntity.apply _)

  implicit lazy val fetchedEORelationshipReads: Reads[FetchedEORelationship] = (
    ((JsPath \ "joins").lazyRead(Reads.seq(fetchedEOJoinReads)) or
      Reads.pure(Seq.empty[FetchedEOJoin])
      ) and
      (JsPath \ "name").read[String] and
      (JsPath \ "definition").readNullable[String]  and
      (JsPath \ "isToMany").read[Boolean] and
      (JsPath \ "destinationEntityName").read[String]
    ) (FetchedEORelationship.apply _)

  implicit lazy val fetchedEOAttributeReads: Reads[FetchedEOAttribute] = (
    (JsPath \ "type").read[String] and
      (JsPath \ "name").read[String]
    ) (FetchedEOAttribute.apply _)

  implicit lazy val fetchedEOJoinReads: Reads[FetchedEOJoin] = (
    (JsPath \ "sourceAttribute").read[FetchedEOAttribute] and
      (JsPath \ "destinationAttribute").read[FetchedEOAttribute]
    ) (FetchedEOJoin.apply _)


  def executeEOModelWS(): Future[EOModel] = {
    val url = d2spaServerBaseUrl + "/EOModel.json";
    Logger.debug("WS " + url)
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      //Logger.debug("Eomodels " + resultBody)
      var entities = List[EOEntity]()

      val modelArray = resultBody.asInstanceOf[JsArray].value
      for (model <- modelArray) {
        val eomodelJsObj = model.asInstanceOf[JsObject]
        val array = (eomodelJsObj \ "entities").get.asInstanceOf[JsArray].value
        //Logger.debug("Entities " + array)

        for (menuRaw <- array) {
          //Logger.debug(menuRaw)
          val obj = menuRaw.validate[FetchedEOEntity]
          obj match {
            case s: JsSuccess[FetchedEOEntity] => {
              val fetchedEOEntity = s.get
              val fetchedRelationships = fetchedEOEntity.relationships
              val relationships = fetchedRelationships.map(
                r => {
                  val joins = r.joins map (join => {
                    EOJoin(join.sourceAttribute.name, join.destinationAttribute.name)
                  })

                  EORelationship(joins.toList, r.name, r.definition, r.isToMany, r.destinationEntityName)
                }).toList
              val attributes: List[String] = fetchedEOEntity.attributes.map {
                a => a.name
              }.toList
              entities = EOEntity(fetchedEOEntity.name, fetchedEOEntity.primaryKeyAttributeNames.toList, attributes, relationships) :: entities
            }
            case e: JsError => Logger.error("Errors: " + JsError.toJson(e).toString())
          }
        }
      }
      //Logger.debug("Entities " + entities)
      EOModel(entities)
    }
  }

  var fetchedEOModel: Option[EOModel] = None

  def receive = LoggingReceive {

      // to the browser
    case GetEOModel(d2wContext, requester) => {
      log.debug("GetEOModel")
      fetchedEOModel match {
        case Some(eomodel) =>
          log.debug("eomodel " + eomodel)
          requester ! EOModelResponse(eomodel, d2wContext)

        case None =>
          executeEOModelWS().map({ eomodel =>
            fetchedEOModel = Some(eomodel)
            requester ! EOModelResponse(eomodel, d2wContext)
          })
      }
    }
  }


}

