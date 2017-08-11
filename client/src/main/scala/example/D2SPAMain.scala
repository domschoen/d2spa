package example

import boopickle.Default._
import diode.dev.{Hooks, PersistStateIDB}
import org.scalajs.dom
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import dom.ext._
import util._

import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._
import japgolly.scalajs.react.vdom.prefix_<^._

import example.css.AppCSS

@JSExport("D2SPA")
object D2SPAMain extends JSApp {
  sealed trait AppPage
  case object Home extends AppPage
  case object DashboardAppPage extends AppPage

  case class NestedTaskAppPage(m: TaskAppPage)  extends AppPage

  sealed trait TaskAppPage
  case object TaskRoot extends TaskAppPage
  case class QueryPage(entity: String) extends TaskAppPage
  case class ListPage(entity: String) extends TaskAppPage

  object TaskAppPage {

    val routes = RouterConfigDsl[TaskAppPage].buildRule { dsl =>
      import dsl._

      val menusConnection = AppCircuit.connect(_.content)


      (emptyRule
        | staticRoute(root, TaskRoot) ~> renderR(ctl => AppCircuit.wrap(_.content)(proxy => D2WPage(ctl, "TOTORO", proxy)))
        | dynamicRouteCT("#task/query/entity" / string(".*").caseClass[QueryPage]) ~> dynRenderR(
                    (m, ctl) => menusConnection(p => D2WPage(ctl,m.entity,p))
                )
        | dynamicRouteCT("#task/list/entity" / string(".*").caseClass[ListPage]) ~> dynRenderR(
                    (m, ctl) => menusConnection(p => D2WListPage(ctl,m.entity,p))
                 )
        )
    }

  }


  val nestedModule =
    TaskAppPage.routes.pmap[AppPage](NestedTaskAppPage){ case NestedTaskAppPage(m) => m }


  val subComp = ReactComponentB[Unit]("printer")
    .render(P => {
      <.div("world")
    }).build

  val config = RouterConfigDsl[AppPage].buildConfig { dsl =>
    import dsl._
    (trimSlashes
      | staticRoute(root,     Home)  ~> render(subComp())
      | nestedModule
      ).notFound(redirectToPage(Home)(Redirect.Replace))
  }
  val baseUrl = BaseUrl.fromWindowOrigin

  val router = Router(baseUrl, config)



  @JSExport
  override def main(): Unit = {
    AppCSS.load

    //AppCircuit.dispatch(InitAppModel)
    ReactDOM.render(router(), dom.document.getElementById("root"))
  }


}
