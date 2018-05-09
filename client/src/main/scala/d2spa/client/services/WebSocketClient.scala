package d2spa.client.services

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import d2spa.client.{SwithDebugMode, UpdateEOInCache}
import d2spa.shared.{FrontendRequest, FrontendResponse}
import org.scalajs.dom
import org.scalajs.dom.raw.{ErrorEvent, Event, MessageEvent, WebSocket}

import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js

@js.native
@JSGlobal
class TextEncoder(utfLabel: js.UndefOr[String] = js.undefined) extends js.Object {
  def encode(str: String): Uint8Array = js.native
}


// google: scala.js websocket send java.nio.ByteBuffer
// ->
// Could be the solution:
// https://github.com/kiritsuku/amora/blob/master/web-ui/src/main/scala/amora/frontend/webui/Connection.scala

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  private lazy val utf8encoder: (String) => Int8Array = {
    val te = new TextEncoder
    // use native TextEncoder
    (str: String) =>
      new Int8Array(te.encode(str))
  }

  private def toArrayBuffer(data: ByteBuffer): js.typedarray.ArrayBuffer = {
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    data.arrayBuffer
  }
  private def toByteBuffer(data: Any): ByteBuffer = {
    val ab = data.asInstanceOf[js.typedarray.ArrayBuffer]
    js.typedarray.TypedArrayBuffer.wrap(ab)
  }

  val ws = new WebSocket(websocketUrl)



  def send(req: d2spa.shared.FrontendRequest): Unit = {
    import boopickle.Default._
    val msg = Pickle.intoBytes(req)
    ws.send(toArrayBuffer(msg))
    dom.console.info(s"Sent request: $req")
  }


  ws.onopen = { (e: Event) ⇒
    // At opening we send a message to the server
    println("Websocket Send message to: " + websocketUrl)
    //val buf: ByteBuffer =       ByteBuffer.wrap("WS Opened".getBytes(StandardCharsets.UTF_8))
    /*val str = "WS Opened"
    val buf  = TypedArrayBuffer.wrap(utf8encoder(str))
    val arrayBuf = toArrayBuffer(buf)

    //val buf: ArrayBuffer = ByteBuffer.wrap("WS Opened".getBytes(StandardCharsets.UTF_8))

    ws.send(arrayBuf)*/
    send(d2spa.shared.FrontendRequest("Pickled Request"))
  }
  /*ws.onerror = (e: ErrorEvent) ⇒ {
    dom.console.error(s"Couldn't create connection to server: ${JSON.stringify(e)}")
  }*/
  ws.onmessage = { (e: MessageEvent) ⇒
    println("Websocket received message: ")

    import boopickle.Default._
    val bytes = toByteBuffer(e.data)
    println("Websocket unpickle: " + bytes.asCharBuffer())

    val resp = Unpickle[d2spa.shared.FrontendResponse].fromBytes(bytes)

    //dom.console.info(s"Received response from server: $resp")
    println("Websocket unpickle: " + resp)

    handleResponse(resp)



    // RefreshedEOs
  }
  ws.onclose = { (event: Event) ⇒
  }

  def send(msg: String) = {
    ws.send(msg)
  }

  def init(): Unit = {
    println("init")
  }

  //override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
  //override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
  def handleResponse(response: FrontendResponse): Unit = {
    //SPACircuit.dispatch(UpdateEOInCache(eo))
    println("Response: " + response)

  }
}
