package controllers

import java.nio.ByteBuffer

import akka.stream.ActorMaterializer
import boopickle.Default._
import com.google.inject.Inject
import play.api.{Configuration, Environment}
import play.api.mvc._
import services.ApiService
import d2spa.shared.Api
import models.UserActor
import play.api.libs.ws._
import play.api.Logger
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// Add actor + web socket
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current
import akka.actor._
import scala.concurrent.Future
import play.libs.Akka
import play.api.mvc.WebSocket
import play.libs.Akka
import akka.actor.ActorSystem
import play.api.Play.materializer

object Router extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

class Application @Inject() (implicit val config: Configuration, env: Environment, ws: WSClient, system: ActorSystem) extends Controller {
  val apiService = new ApiService(config,ws)
  val UID = "uid"
  var counter = 0;
  //implicit val materializer = ActorMaterializer()


  def index = Action {
    Ok(views.html.index("D2SPA"))
  }

  // called by the browser
  def autowireApi(path: String) = Action.async(parse.raw) {
    implicit request =>
      Logger.debug(s"Request path: $path")

      // get the request body as ByteStringthenApply
      val b = request.body.asBytes(parse.UNLIMITED).get

      // call Autowire route
      Router.route[Api](apiService)(
        autowire.Core.Request(path.split("/"), Unpickle[Map[String, ByteBuffer]].fromBytes(b.asByteBuffer))
      ).map(buffer => {
        val data = Array.ofDim[Byte](buffer.remaining())
        buffer.get(data)
        Ok(data)
      })
  }


  // Log
  def logging = Action(parse.anyContent) {
    implicit request =>
      request.body.asJson.foreach { msg =>
        Logger.debug(s"CLIENT - $msg")
      }
      Ok("")
  }


  def ws() = WebSocket.accept[akka.util.ByteString, akka.util.ByteString] {request =>
    println("WebSocket Received ")
    ActorFlow.actorRef(out => MyWebSocketActor.props(out))
  }


  object MyWebSocketActor {
    def props(out: ActorRef) = Props(new MyWebSocketActor(out))
  }

  class MyWebSocketActor(out: ActorRef) extends Actor {
    def receive = {
      case msg: akka.util.ByteString =>

        val asString = (msg.map(_.toChar)).mkString

        println("WebSocket Received message: " + asString)


        //val d = Pickle.intoBytes[WebMessages.Message](result)

        //val eo = EO()
        //apiService.

        //val worldFuture = Future { getLocationTrends(twitter, woeidWorld) }

        //out ! ("I received your message: " + msg)
    }
  }

}
