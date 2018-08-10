package d2spa.client.services

import d2spa.client._
import d2spa.shared.{FrontendRequest, FrontendResponse}
import d2spa.shared.WebSocketMessages._
import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}

import d2spa.client
import org.scalajs.dom
import org.scalajs.dom._
import d2spa.shared.{Test3,Test4,Test5,Test10}

import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.js.timers._


// google: scala.js websocket send java.nio.ByteBuffer
// ->
// Could be the solution:
// https://github.com/kiritsuku/amora/blob/master/web-ui/src/main/scala/amora/frontend/webui/Connection.scala

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  case class Socket(url: String)(onMessage: (MessageEvent) => _) {
    private val socket: WebSocket = new dom.WebSocket(url = url)

    def send(msg: WebSocketMsgIn): Unit = {
      import scala.scalajs.js.typedarray.TypedArrayBufferOps._

      val bytes = Pickle.intoBytes(msg).arrayBuffer()
      println("Send " + msg)
      println("Send " + bytes.byteLength + " bytes")
      socket.send(bytes)
    }

    socket.onopen = (e: Event) => { MyCircuit.dispatch(SocketReady) }
    socket.onclose = (e: CloseEvent) => { dom.console.log(s"Socket closed. Reason: ${e.reason} (${e.code})") }
    socket.onerror = (e: Event) => { dom.console.log(s"Socket error! ${e}") }
    socket.onmessage = onMessage
  }


  object Socket {
    def blobReader(): FileReader = {
      val reader = new FileReader()
      reader.onerror = (e: Event) => { dom.console.log(s"Error in blobReader: ${reader.error}") }
      reader.onload = (e: UIEvent) => {
        reader.result match {
          case buf: ArrayBuffer =>
            Unpickle[WebSocketMsgOut].fromBytes(TypedArrayBuffer.wrap(buf)) match {
              case FetchedEOModel(eomodel) => MyCircuit.dispatch(SetEOModelThenFetchMenu(eomodel))
              case FetchedMenus(menus) => MyCircuit.dispatch(SetMenus(menus))
              case RuleResults(ruleResults) => MyCircuit.dispatch(client.SetJustRuleResults(ruleResults))
              case FetchedObjectsMsgOut(eos) => MyCircuit.dispatch(client.FetchedObjectsForEntity(eos))
              case FetchedObjectsForListMsgOut(entityName, eos) => MyCircuit.dispatch(client.SearchResult(entityName, eos))
              case SavingResponseMsgOut(eo) => MyCircuit.dispatch(client.SavingEO(eo))
              case DeletingResponseMsgOut(eo) => MyCircuit.dispatch(client.DeletingEO(eo))
              case DebugConfMsg(showDebugButton) => MyCircuit.dispatch(SetDebugConfiguration(DebugConf(showDebugButton)))
            }
          case _ => // ignored
        }
      }

      reader
    }
  }




}
