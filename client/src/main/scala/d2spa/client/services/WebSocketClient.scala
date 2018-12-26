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

import d2spa.client.logger._

// google: scala.js websocket send java.nio.ByteBuffer
// ->
// Could be the solution:
// https://github.com/kiritsuku/amora/blob/master/web-ui/src/main/scala/amora/frontend/webui/Connection.scala

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  case class Socket(url: String, d2wContext: D2WContext)(onMessage: (MessageEvent) => _) {
    println("  SOCKETE SOCKETE SOCKETE")
    private val socket: WebSocket = new dom.WebSocket(url = url)

    def send(msg: WebSocketMsgIn): Unit = {
      import scala.scalajs.js.typedarray.TypedArrayBufferOps._

      val bytes = Pickle.intoBytes(msg).arrayBuffer()
      log.finest("Send " + msg)
      log.finest("Send " + bytes.byteLength + " bytes")
      socket.send(bytes)
    }

    socket.onopen = (e: Event) => { MyCircuit.dispatch(SetPageForSocketReady(d2wContext)) }
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
              case FetchedEOModel(eomodel,d2wContext) => MyCircuit.dispatch(SetEOModelThenFetchMenu(eomodel,d2wContext))
              case FetchedMenus(menus, d2wContext) => MyCircuit.dispatch(SetMenus(menus, d2wContext))
              case RuleResults(ruleResults) => MyCircuit.dispatch(client.SetJustRuleResults(ruleResults))


              case CompletedEOMsgOut(d2wContext, eo, ruleResultsOpt) => MyCircuit.dispatch(client.CompletedEO(d2wContext,eo,ruleResultsOpt))
              case FetchedObjectsMsgOut(entityName, eos, ruleResultsOpt) => MyCircuit.dispatch(client.FetchedObjectsForEntity(entityName,eos,ruleResultsOpt))
              case FetchedObjectsForListMsgOut(fs, eos) => MyCircuit.dispatch(client.SearchResult(fs, eos))
              case SavingResponseMsgOut(eo) => MyCircuit.dispatch(client.SavingEO(eo))
              case DeletingResponseMsgOut(eo) => MyCircuit.dispatch(client.DeletingEO(eo))
              case DebugConfMsg(showDebugButton, d2wContext) => MyCircuit.dispatch(SetDebugConfiguration(DebugConf(showDebugButton), d2wContext))
            }
          case _ => // ignored
        }
      }

      reader
    }
  }




}
