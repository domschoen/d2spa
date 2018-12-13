package d2spa.client

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import d2spa.client.components.GlobalStyles
import d2spa.client.logger._
import d2spa.client.services.{MyCircuit, WebSocketClient}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import CssSettings._
import scalacss.ScalaCssReact._
import d2spa.client.logger._
import d2spa.client.services.WebSocketClient.{Socket, websocketUrl}
import d2spa.shared.{EO, EOEntity, EOValue, TaskDefine}
import diode.react.ModelProxy
import org.scalajs.dom.{Blob, MessageEvent}
import org.scalajs.dom.raw.{ErrorEvent, Event, MessageEvent, WebSocket}

@JSExportTopLevel("SPAMain")
object SPAMain extends js.JSApp {

  // Define the locations (pages) used in this application
  sealed trait AppPage
  case object Home extends AppPage
  case object DashboardAppPage extends AppPage

  case class NestedTaskAppPage(m: TaskAppPage)  extends AppPage

  sealed trait TaskAppPage
  case object TaskRoot extends TaskAppPage
  case class QueryPage(entity: String) extends TaskAppPage
  case class ListPage(entity: String) extends TaskAppPage
  case class EditPage(entity: String,  pk: Int) extends TaskAppPage
  case class NewEOPage(entityName: String) extends TaskAppPage
  case class InspectPage(entity: String, pk: Int) extends TaskAppPage

  object TaskAppPage {



    val routes = RouterConfigDsl[TaskAppPage].buildRule { dsl =>
      import dsl._

      val menusConnection = MyCircuit.connect(_.content)


      (emptyRule

        | staticRedirect(root) ~> redirectToPage(QueryPage("Project"))(Redirect.Replace)
        /*| staticRoute(root, TaskRoot) ~> renderR(ctl => {
            AfterEffectRouter.setCtl(ctl)
            menusConnection(p => {
              val d2wContext = D2WContext(entityName = Some("Project"), task =  Some(TaskDefine.query))
              D2WQueryPage(ctl, d2wContext, p)
            })
          })*/

        | dynamicRouteCT("#task/query/entity" / string(".*").caseClass[QueryPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => {
                val d2wContext = p.value.previousPage match {
                  case Some(previousPage) =>
                    //println("SPAMain | Router previous page " + previousPage)
                    previousPage
                  case None =>
                    //println("SPAMain | Router no previous Page")
                    val firstD2WContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.query))
                    //p.dispatchCB(RegisterPreviousPage(firstD2WContext))
                    firstD2WContext

                }
                D2WQueryPage(ctl, d2wContext, p)
              })
            }
          )

        | dynamicRouteCT("#task/list/entity" / string(".*") .caseClass[ListPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => {
                val d2wContext = p.value.previousPage match {
                  case Some(previousPage) =>
                    previousPage
                  case None =>
                    val firstD2WContext =D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.list))
                    //p.dispatchCB(RegisterPreviousPage(firstD2WContext))
                    firstD2WContext

                }
                D2WListPage(ctl, d2wContext, p)
              })
            }
          )
        | dynamicRouteCT(("#task/edit/entity" / string(".*") / int ).caseClass[EditPage]) ~> dynRenderR(
             (m, ctl) => {
                AfterEffectRouter.setCtl(ctl)
               menusConnection(p => {
                 val d2wContext = p.value.previousPage match {
                   case Some(previousPage) =>
                     previousPage
                   case None =>
                     val firstD2WContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.edit), pk = Some(m.pk))
                     //p.dispatchCB(RegisterPreviousPage(firstD2WContext))
                     firstD2WContext


                 }
                 D2WEditPage(ctl, d2wContext, p)
               })
              }
           )
        | dynamicRouteCT(("#task/inspect/entity" / string(".*") / int).caseClass[InspectPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)

              menusConnection(p => {
                val d2wContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.inspect), pk = Some(m.pk))
                D2WEditPage(ctl, d2wContext, p)
              })
            }
          )

        // To use only to enter the app
        | dynamicRouteCT(("#task/new/entity" / string(".*")).caseClass[NewEOPage]) ~> dynRenderR(
          (m, ctl) => {
            AfterEffectRouter.setCtl(ctl)
            menusConnection(p => {
              val d2wContext = D2WContext(entityName = Some(m.entityName), task = Some(TaskDefine.edit), eo = None)
              D2WEditPage(ctl, d2wContext, p)
            })
          }
        )
      )

    }
  }

  // case class EO(entity: EOEntity, values: Map[String,EOValue], memID: Option[Int] = None, validationError: Option[String] = None)


  val nestedModule =
    TaskAppPage.routes.pmap[AppPage](NestedTaskAppPage){ case NestedTaskAppPage(m) => m }


  val subComp = ScalaComponent.builder[Unit]("printer")
    .render(P => {
      <.div("world")
    }).build

  val config = RouterConfigDsl[AppPage].buildConfig { dsl =>
    import dsl._
    (trimSlashes
      | nestedModule
      ).notFound(redirectToPage(Home)(Redirect.Replace))
  }
  val baseUrl = BaseUrl.fromWindowOrigin

  val router = Router(baseUrl, config)

  val socket = Socket(websocketUrl)((event: MessageEvent) => event.data match {
    case blob: Blob =>
      //println("Will read socket")
      Socket.blobReader().readAsArrayBuffer(blob) //the callbacks in blobReader take care of what happens with the data.
      //Socket.blobReader.abort()
    case _ => dom.console.log("Error on receive, should be a blob.")
  })

  @JSExport
  def main(): Unit = {
    // FINE level -> severe, warning, info

    //log.severe("Severe")
    //log.warning("warning")
    //log.info("info")
    //log.config("config")
    //log.fine("fine")
    //log.finer("finer")
    //log.finest("finest")

    // send log messages also to the server
    //log.enableServerLogging("/logging")
    log.info("This message goes to server as well")


    // create stylesheet
    GlobalStyles.addToDocument()



    // create the router
    val router = Router(BaseUrl.until_#, config)
    // tell React to render the router in the document body
    router().renderIntoDOM(dom.document.getElementById("root"))
  }



  def setupWebSocket() = {


  }
}
