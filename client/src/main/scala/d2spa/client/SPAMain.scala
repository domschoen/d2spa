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
import d2spa.shared._
import diode.react.ModelProxy
import org.scalajs.dom.{Blob, MessageEvent}
import org.scalajs.dom.raw.{ErrorEvent, Event, MessageEvent, WebSocket}

import scala.scalajs.js.JSApp

@JSExportTopLevel("SPAMain")
object SPAMain   extends  js.JSApp {

  // Define the locations (pages) used in this application
  sealed trait AppPage
  case object Home extends AppPage
  case object DashboardAppPage extends AppPage

  case class NestedTaskAppPage(m: TaskAppPage)  extends AppPage

  sealed trait TaskAppPage
  case object TaskRoot extends TaskAppPage
  case class SVGTrialPage(entity: String) extends TaskAppPage
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
        // staticRoute("#hello", Hello) ~> render(HelloComponent())
        //| staticRoute("#d3", D3Trial) ~> renderR(ctl => menusConnection(D3Trial(ctl)))
        //| staticRedirect("#d3") ~> redirectToPage(D3Trial)(Redirect.Replace)
        | dynamicRouteCT("#svg" / string(".*").caseClass[SVGTrialPage]) ~> dynRenderR(
        (m, ctl) => {
          SVGTrial(ctl)
        })

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
                    PageContext(d2wContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.query)))
                }
                if (!p.value.appConfiguration.socketReady) {
                  WebSocketClient.setSocket(Some(d2wContext))
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
                    val firstD2WContext = PageContext(d2wContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.list)))
                    //p.dispatchCB(RegisterPreviousPage(firstD2WContext))
                    firstD2WContext

                }
                if (!p.value.appConfiguration.socketReady) {
                  WebSocketClient.setSocket(Some(d2wContext))
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
                     val firstD2WContext = PageContext(d2wContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.edit)),
                       eo = Some(EOContainer(
                        EO(
                          entityName = m.entity,
                          pk = EOPk(List(m.pk))
                        )
                       ))
                     )
                     firstD2WContext
                 }
                 if (!p.value.appConfiguration.socketReady) {
                   WebSocketClient.setSocket(Some(d2wContext))
                 }
                 D2WEditPage(ctl, d2wContext, p)
               })
              }
           )
        | dynamicRouteCT(("#task/inspect/entity" / string(".*") / int).caseClass[InspectPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)

              menusConnection(p => {

                val d2wContext = PageContext(d2wContext = D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.inspect)),
                  eo = Some(EOContainer(
                  EO(entityName = m.entity, pk = EOPk(List(m.pk)))
                )))
                if (!p.value.appConfiguration.socketReady) {
                  WebSocketClient.setSocket(Some(d2wContext))
                }
                D2WEditPage(ctl, d2wContext, p)
              })
            }
          )

        // To use only to enter the app
        | dynamicRouteCT(("#task/new/entity" / string(".*")).caseClass[NewEOPage]) ~> dynRenderR(
          (m, ctl) => {
            AfterEffectRouter.setCtl(ctl)
            menusConnection(p => {
              val d2wContext = PageContext(d2wContext = D2WContext(entityName = Some(m.entityName), task = Some(TaskDefine.edit)),
                eo = None)
              if (!p.value.appConfiguration.socketReady) {
                WebSocketClient.setSocket(Some(d2wContext))
              } else {
                p.dispatchCB(PrepareEODisplay(d2wContext))
              }
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



}
