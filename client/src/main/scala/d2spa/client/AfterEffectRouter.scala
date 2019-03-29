package d2spa.client

import scala.concurrent.Future
import japgolly.scalajs.react.extra.router._
import d2spa.client.SPAMain._
import d2spa.client.logger.log
import d2spa.shared._

import scala.concurrent.ExecutionContext.Implicits.global

object AfterEffectRouter {
  val singleton = new AfterEffectRouter()

  def setPageForTaskAndEOAndEntity(pageContext: PageContext): Future[diode.NoAction.type] = {


    log.finest("AfterEffectRouter " + pageContext)
    val d2wContext = pageContext.d2wContext
    val entityName = d2wContext.entityName.get
    d2wContext.task.get match {
      case "query" => setRouterToPage(QueryPage(entityName))
      case "list" => setRouterToPage(ListPage(entityName))
      case "edit" => setEOPageWithContext(pageContext)
      case "inspect" => setEOPageWithContext(pageContext)
      case _ => setRouterToPage(QueryPage(entityName))
    }
  }

  def setEOPageWithContext(pageContext: PageContext): Future[diode.NoAction.type] = {
    val d2wContext = pageContext.d2wContext
    val entityName = d2wContext.entityName.get
    val taskName = d2wContext.task.get
    pageContext.eo match {
      case Some(eoContaining) =>
        val eo = eoContaining.eo
        val pk = eo.pk.pks.head
        if (pk < 0) {
          val page = EditPage(entityName,pk)
          log.finest("Set Router to page " + page)
          setRouterToPage(page)
        } else {
          val page = if (taskName.equals(TaskDefine.edit)) EditPage(entityName, pk) else InspectPage(entityName, pk)
          setRouterToPage(page)
        }

      case _ => {
        setRouterToPage(NewEOPage(entityName))
      }
    }
  }

  def setQueryPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(QueryPage(entity))
  def setListPageForEntity( entity: String) : Future[diode.NoAction.type] = setRouterToPage(ListPage(entity))
  def setEditPageForEntity( entity: String, pk: Int) : Future[diode.NoAction.type] = setRouterToPage(EditPage(entity, pk))
  def setInspectPageForEntity( entity: String, pk: Int) : Future[diode.NoAction.type] = setRouterToPage(InspectPage(entity, pk))


  def setRouterToPage(page: TaskAppPage): Future[diode.NoAction.type] = {
    log.finest("AfterEffectRouter | setRouterToPage " + page)
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

