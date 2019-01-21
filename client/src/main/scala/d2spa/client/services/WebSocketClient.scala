package d2spa.client.services

import d2spa.client._
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.client
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.js.timers._
import d2spa.client.logger._
import org.scalajs.dom.raw.MessageEvent

// google: scala.js websocket send java.nio.ByteBuffer
// ->
// Could be the solution:
// https://github.com/kiritsuku/amora/blob/master/web-ui/src/main/scala/amora/frontend/webui/Connection.scala

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  var socketOpt: Option[Socket] = None

  def setSocket(d2wContext: Option[PageContext]) = {
    socketOpt = Some(Socket(websocketUrl, d2wContext)((event: MessageEvent) => event.data match {
      case blob: Blob =>
        //println("Will read socket")
        Socket.blobReader().readAsArrayBuffer(blob) //the callbacks in blobReader take care of what happens with the data.
      //Socket.blobReader.abort()
      case _ => dom.console.log("Error on receive, should be a blob.")
    }))

  }

  def send(msg: WebSocketMsgIn): Unit = {
    socketOpt match {
      case Some(socket) =>
        if (socket.isClosed) {
          setSocket(None)
        }
      case None =>
        setSocket(None)
    }
    socketOpt.get.send(msg)
  }


  case class Socket(url: String, d2wContextOpt: Option[PageContext])(onMessage: (MessageEvent) => _) {
    println("  SOCKETE SOCKETE SOCKETE")

    var isClosed = true

    private val socket: WebSocket = new dom.WebSocket(url = url)

    def send(msg: WebSocketMsgIn): Unit = {
      import scala.scalajs.js.typedarray.TypedArrayBufferOps._
      val bytes = Pickle.intoBytes(msg).arrayBuffer()
      log.finest("Send " + msg)
      log.finest("Send " + bytes.byteLength + " bytes")
      socket.send(bytes)
    }


      socket.onopen = (e: Event) => {
        d2wContextOpt match {
          case Some(d2wContext) =>
            MyCircuit.dispatch(SetPageForSocketReady(d2wContext))
          case None => ()
        }
        isClosed = false
      }
      socket.onclose = (e: CloseEvent) => {
        dom.console.log(s"Socket closed. Reason: ${e.reason} (${e.code})")
        isClosed = true
      }
      socket.onerror = (e: Event) => {
        dom.console.log(s"Socket error! ${e}")
      }
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
              case RuleRequestResponseMsg(d2wContext, ruleResults) => MyCircuit.dispatch(client.SetMetaData(d2wContext, ruleResults))
              case RuleRequestForAppInitResponseMsg(d2wContext, ruleResults, eoOpt) => MyCircuit.dispatch(client.SetRulesForPrepareEO(d2wContext, ruleResults, eoOpt))

              case RulesForSearchResultResponseMsgOut(fs,eos,ruleResultsOpt) => MyCircuit.dispatch(client.SearchResultWithRuleResults(fs,eos,ruleResultsOpt))
              case CompletedEOMsgOut(d2wContext, eo, ruleResultsOpt) => MyCircuit.dispatch(client.CompletedEO(d2wContext,eo,ruleResultsOpt))
              case FetchedObjectsMsgOut(entityName, eos, ruleResultsOpt) => MyCircuit.dispatch(client.FetchedObjectsForEntity(entityName,eos,ruleResultsOpt))
              case FetchedObjectsForListMsgOut(fs, eos) => MyCircuit.dispatch(client.SearchResult(fs, eos))
              case SavingResponseMsgOut(d2wContext: D2WContext, eo: EO, ruleResults: Option[List[RuleResult]]) =>
                MyCircuit.dispatch(client.SavingEO(d2wContext, eo, ruleResults))
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
