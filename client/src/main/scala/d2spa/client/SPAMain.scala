package d2spa.client

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import d2spa.client.components.GlobalStyles
import d2spa.client.logger._
import d2spa.client.services.{SPACircuit, WebSocketClient}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import CssSettings._
import scalacss.ScalaCssReact._
import d2spa.client.logger._
import d2spa.shared.{EOValue, TaskDefine, EO, EOEntity}
import diode.react.ModelProxy
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
  case class NewEOPage(entity: String) extends TaskAppPage
  case class InspectPage(entity: String, pk: Int) extends TaskAppPage

  object TaskAppPage {



    val routes = RouterConfigDsl[TaskAppPage].buildRule { dsl =>
      import dsl._

      val menusConnection = SPACircuit.connect(_.content)


      (emptyRule
        | staticRoute(root, TaskRoot) ~> renderR(ctl => SPACircuit.wrap(_.content)(proxy => D2WQueryPage(ctl, D2WContext(entityName = Some("Project"), task = Some(TaskDefine.query)), proxy)))

        | dynamicRouteCT("#task/query/entity" / string(".*").caseClass[QueryPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => {
                val d2wContext = p.value.previousPage match {
                  case Some(previousPage) =>
                    previousPage
                  case None =>
                    D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.query))

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
                    D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.list))

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
                     D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.edit), pk = Some(m.pk))

                 }
                 D2WEditPage(ctl, d2wContext, p)
               })
              }
           )
        | dynamicRouteCT(("#task/inspect/entity" / string(".*") / int).caseClass[InspectPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)

              menusConnection(p => {
                val d2wContext = p.value.previousPage match {
                  case Some(previousPage) =>
                    previousPage
                  case None =>
                    D2WContext(entityName = Some(m.entity), task =  Some(TaskDefine.inspect), pk = Some(m.pk))

                }
                D2WEditPage(ctl, d2wContext, p)
              })
            }
          )
        | dynamicRouteCT(("#task/new/entity" / string(".*")).caseClass[NewEOPage]) ~> dynRenderR(
          (m, ctl) => {
            AfterEffectRouter.setCtl(ctl)
            menusConnection(p => {
              val d2wContext = p.value.previousPage.get
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
    log.warn("Application starting")
    // send log messages also to the server
    //log.enableServerLogging("/logging")
    log.info("This message goes to server as well")

    // create stylesheet
    GlobalStyles.addToDocument()

    //WebSocketClient.init()
    SPACircuit.dispatch(InitClient)

    // create the router
    val router = Router(BaseUrl.until_#, config)
    // tell React to render the router in the document body
    router().renderIntoDOM(dom.document.getElementById("root"))
  }



  def setupWebSocket() = {


  }
}
