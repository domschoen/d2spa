package d2spa.client

import scala.concurrent.Future
import japgolly.scalajs.react.extra.router._
import d2spa.client.SPAMain._
import d2spa.shared.{EO, EOEntity, EOValueUtils}

import scala.concurrent.ExecutionContext.Implicits.global

object AfterEffectRouter {
  val singleton = new AfterEffectRouter()

  def setPageForTaskAndEOAndEntity(task: String, pkOpt: Option[Int], memIDOpt: Option[Int], entity: String): Future[diode.NoAction.type] = {


    println("Set " + task + " page " + entity + " pk " + pkOpt)
    task match {
      case "query" => setRouterToPage(QueryPage(entity))
      case "list" => setRouterToPage(ListPage(entity))
      case "edit" => {
        pkOpt match {
          case Some(pk) => setRouterToPage(EditPage(entity, pk))
          case _ =>
            memIDOpt match {
              case Some(memID) => setRouterToPage(NewPage(entity, memID))
              case _ => setRouterToPage(QueryPage(entity))
            }
        }
      }
      // To be refactored : same code as above
      case "inspect" => {
        pkOpt match {
          case Some(pk) => setRouterToPage(InspectPage(entity, pk))
          case _ => setRouterToPage(QueryPage(entity))
        }

      }
      case _ => setRouterToPage(QueryPage(entity))
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

