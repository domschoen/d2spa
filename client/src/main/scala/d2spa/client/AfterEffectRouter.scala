package d2spa.client

import scala.concurrent.Future
import japgolly.scalajs.react.extra.router._
import d2spa.client.SPAMain._
import d2spa.client.logger.log
import d2spa.shared._

import scala.concurrent.ExecutionContext.Implicits.global

object AfterEffectRouter {
  val singleton = new AfterEffectRouter()

  def setPageForTaskAndEOAndEntity(d2WContext: D2WContext): Future[diode.NoAction.type] = {


    log.debug("AfterEffectRouter " + d2WContext)
    val entityName = d2WContext.entityName.get
    d2WContext.task.get match {
      case "query" => setRouterToPage(QueryPage(entityName))
      case "list" => setRouterToPage(ListPage(entityName))
      case "edit" => setEOPageWithContext(d2WContext)
      case "inspect" => setEOPageWithContext(d2WContext)
      case _ => setRouterToPage(QueryPage(entityName))
    }
  }

  def setEOPageWithContext(d2WContext: D2WContext): Future[diode.NoAction.type] = {
    val entityName = d2WContext.entityName.get
    val taskName = d2WContext.task.get
    d2WContext.eo match {
      case Some(eo) =>
        val pk = eo.pk
        if (pk < 0) {
          val page = NewEOPage(entityName)
          log.debug("Set Router to page " + page)
          setRouterToPage(page)
        } else {
          val page = if (taskName.equals(TaskDefine.edit)) EditPage(entityName, pk) else InspectPage(entityName, pk)
          setRouterToPage(page)
        }

      case _ => {
        setRouterToPage(QueryPage(entityName))
      }
    }
  }

  def setQueryPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(QueryPage(entity))
  def setListPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(ListPage(entity))
  def setEditPageForEntity( entity: String, pk: Int) : Future[diode.NoAction.type] = setRouterToPage(EditPage(entity, pk))
  def setInspectPageForEntity( entity: String, pk: Int) : Future[diode.NoAction.type] = setRouterToPage(InspectPage(entity, pk))


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

