package example

import scala.concurrent.Future
import japgolly.scalajs.react.extra.router._
import example.D2SPAMain.{QueryPage,ListPage,EditPage,InspectPage, TaskAppPage}
import scala.concurrent.ExecutionContext.Implicits.global

object AfterEffectRouter {
  val singleton = new AfterEffectRouter()

  def setQueryPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(QueryPage(entity))
  def setListPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(ListPage(entity))
  def setEditPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(EditPage(entity))
  def setInspectPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(InspectPage(entity))


  def setRouterToPage(page: TaskAppPage): Future[diode.NoAction.type] = {
    // RouterCtl.set returns a Callback[Unit]
    // Callback implemented as functions that return a CallbackTo[Unit]
    Future {
      ctl().set(page).runNow()
      diode.NoAction
    }
  }

  def setCtl(router: RouterCtl[TaskAppPage]): Unit = {
    singleton.routerCtl = router
  }

  def ctl() : RouterCtl[TaskAppPage] = singleton.routerCtl

}

class AfterEffectRouter {
  var routerCtl: RouterCtl[TaskAppPage] = null

}

