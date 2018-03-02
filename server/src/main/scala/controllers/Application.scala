package controllers

import java.nio.ByteBuffer

import boopickle.Default._
import com.google.inject.Inject
import play.api.{Configuration, Environment}
import play.api.mvc._
import services.ApiService
import d2spa.shared.Api
import play.api.libs.ws._
import play.api.Logger


import scala.concurrent.ExecutionContext.Implicits.global

object Router extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

class Application @Inject() (implicit val config: Configuration, env: Environment, ws: WSClient) extends Controller {
  val apiService = new ApiService(config,ws)


  def index = Action {
    Ok(views.html.index("D2SPA"))
  }

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

  def logging = Action(parse.anyContent) {
    implicit request =>
      request.body.asJson.foreach { msg =>
        Logger.debug(s"CLIENT - $msg")
      }
      Ok("")
  }
}
