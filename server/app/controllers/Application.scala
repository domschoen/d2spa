package controllers

import java.nio.ByteBuffer

import models._
import akka.stream.ActorMaterializer
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import boopickle.Default._
import d2spa.shared.WebSocketMessages.{WebSocketMsgIn, WebSocketMsgOut}
import com.google.inject.Inject
import com.google.inject.name.Named
import play.api._
import play.api.http.websocket.{BinaryMessage, CloseCodes, CloseMessage, Message}
import play.api.libs.streams.{ActorFlow, AkkaStreams}
import play.api.mvc.WebSocket.MessageFlowTransformer
import play.api.mvc._
import services.ApiService
import d2spa.shared.{Api, FrontendResponse}
import play.api.libs.ws._
import play.api.Logger

import scala.util.{Failure, Success}


class Application @Inject()(@Named("node-actor") nodeActor: ActorRef)
                           (implicit config: Configuration, system: ActorSystem,

                                                                                  environment: Environment, ws: WSClient) extends Controller {
  implicit val webSocketTransformer = new MessageFlowTransformer[WebSocketMsgIn, WebSocketMsgOut] {
    override def transform(flow: Flow[WebSocketMsgIn, WebSocketMsgOut, _]): Flow[Message, Message, _] = {
      AkkaStreams.bypassWith[Message, WebSocketMsgIn, Message](Flow[Message] collect {
        case BinaryMessage(data) => Unpickle[WebSocketMsgIn].tryFromBytes(data.asByteBuffer) match {
          case Success(msg) => Left(msg)
          case Failure(err) => Right(CloseMessage(CloseCodes.Unacceptable, s"Error with transfer: $err"))
        }
        case _ => Right(CloseMessage(CloseCodes.Unacceptable, "This WebSocket only accepts binary."))
      })(flow.map { msg =>
        val bytes = ByteString.fromByteBuffer(Pickle.intoBytes(msg))
        BinaryMessage(bytes)
      })
    }
  }
  val apiService = new ApiService(config,ws)
  val UID = "uid"
  var counter = 0;
  implicit val materializer = ActorMaterializer()


  def index = Action {
    Ok(views.html.index("D2SPA"))
  }



  // Log
  def logging = Action(parse.anyContent) {
    implicit request =>
      request.body.asJson.foreach { msg =>
        Logger.debug(s"CLIENT - $msg")
      }
      Ok("")
  }


  def ws = WebSocket.accept[WebSocketMsgIn, WebSocketMsgOut] { request =>
    ActorFlow.actorRef(out => WebSocketActor.props(out, nodeActor))
  }



}
