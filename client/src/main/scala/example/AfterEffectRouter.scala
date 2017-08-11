package example

import scala.concurrent.Future
import japgolly.scalajs.react.extra.router._
import example.D2SPAMain.{QueryPage,ListPage,TaskAppPage}
import scala.concurrent.ExecutionContext.Implicits.global

object AfterEffectRouter {

  def setQueryPageForEntityAsFuture(router: RouterCtl[TaskAppPage], entity: String) : Future[diode.NoAction.type] = setRouterToPageAsFuture(router,QueryPage(entity))
  def setQueryPageForEntity(router: RouterCtl[TaskAppPage], entity: String) : diode.NoAction.type = setRouterToPage(router,QueryPage(entity))


  def setRouterToPageAsFuture(router: RouterCtl[TaskAppPage], page: TaskAppPage): Future[diode.NoAction.type] = {
    // RouterCtl.set returns a Callback[Unit]
    // Callback implemented as functions that return a CallbackTo[Unit]
    Future {
      router.set(page).runNow()
      diode.NoAction
    }
  }

  def setRouterToPage(router: RouterCtl[TaskAppPage], page: TaskAppPage): diode.NoAction.type = {
    router.set(page).runNow()
    diode.NoAction
  }


}

