package d2spa.client.services

import java.nio.ByteBuffer

import boopickle.Default.{Pickle, Pickler, Unpickle}
import d2spa.client.SwithDebugMode
import org.scalajs.dom
import org.scalajs.dom.raw.{ErrorEvent, Event, MessageEvent, WebSocket}

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  val chat = new WebSocket(websocketUrl)

  chat.onopen = { (event: Event) ⇒
    // At opening we send a message to the server
    println("Websocket Send message to: " + websocketUrl)
    chat.send("WS Opened")
  }
  chat.onerror = { (event: ErrorEvent) ⇒
  }
  chat.onmessage = { (event: MessageEvent) ⇒
    println("Websocket received message: " + event.data.toString)
    SPACircuit.dispatch(SwithDebugMode)

  }
  chat.onclose = { (event: Event) ⇒
  }

  def send(msg: String) = {
    chat.send(msg)
  }

  def init(): Unit = {
    println("init")
  }

  //override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
  //override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)

}
