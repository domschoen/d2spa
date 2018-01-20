package d2spa.client

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import d2spa.client.components.GlobalStyles
import d2spa.client.logger._
import d2spa.client.services.SPACircuit

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import CssSettings._

import scalacss.ScalaCssReact._
import d2spa.client.logger._
import d2spa.shared.D2WContext

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
  case class EditPage(entity: String) extends TaskAppPage
  case class InspectPage(entity: String) extends TaskAppPage

  object TaskAppPage {

    val routes = RouterConfigDsl[TaskAppPage].buildRule { dsl =>
      import dsl._

      val menusConnection = SPACircuit.connect(_.content)


      (emptyRule
        | staticRoute(root, TaskRoot) ~> renderR(ctl => SPACircuit.wrap(_.content)(proxy => D2WQueryPage(ctl, D2WContext(Some("Project"),Some("query")), proxy)))

        | dynamicRouteCT("#task/query/entity" / string(".*").caseClass[QueryPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => {
                D2WQueryPage(ctl, D2WContext(Some(m.entity), Some("query")), p)
              })
            }
          )

        | dynamicRouteCT("#task/list/entity" / string(".*").caseClass[ListPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => {
                D2WListPage(ctl, D2WContext(Some(m.entity),Some("list")), p)
              })
            }
          )
        | dynamicRouteCT("#task/edit/entity" / string(".*").caseClass[EditPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => D2WEditPage(ctl, D2WContext(Some(m.entity),Some("edit")), p ))
            }
        )
        | dynamicRouteCT("#task/inspect/entity" / string(".*").caseClass[InspectPage]) ~> dynRenderR(
            (m, ctl) => {
              AfterEffectRouter.setCtl(ctl)
              menusConnection(p => D2WEditPage(ctl, D2WContext(Some(m.entity),Some("inspect")), p))
            }
          )
        )
    }

  }
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
    log.enableServerLogging("/logging")
    log.info("This message goes to server as well")

    // create stylesheet
    GlobalStyles.addToDocument()

    //println( GlobalStyles.render[String] )
    SPACircuit.dispatch(InitClient)

    // create the router
    val router = Router(BaseUrl.until_#, config)
    // tell React to render the router in the document body
    router().renderIntoDOM(dom.document.getElementById("root"))
  }
}
