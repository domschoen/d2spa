package d2spa.client.services

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate
import diode.ActionResult.ModelUpdateEffect
import d2spa.shared._
import boopickle.Default._
import d2spa.client.EOCacheUtils.updatedMemCacheByRemovingEO

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import japgolly.scalajs.react.extra.router.RouterCtl
import d2spa.client.SPAMain.TaskAppPage
import d2spa.client._
import d2spa.client.logger._

import scala.collection.immutable.Set

/*import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import japgolly.scalajs.react.extra.router._*/

import d2spa.client.AppModel

import d2spa.shared.{Menus, EntityMetaData, PropertyMetaInfo, EO, EOValue}

// The First page displayed is the Query page. When the query page is mounted, it calls the server for the entities to be displayed.
// This is done with an action "InitMetaData" which trigger "" on the server "getMetaData"
// When data get back from server, SetMetaData is called, followed by an action "InitMenu" which goes to server and trigger a Menu.json on D2SPAServer
// D2SPAServer return a list of entities
//
object MyCircuit extends Circuit[AppModel] with ReactConnector[AppModel] {
  // define initial value for the application model
  override protected def initialModel = AppModel.bootingModel

  /*var _bootingModel = AppModel.bootingModel

  def setInitialPage(d2wContext : D2WContext) = {
    _bootingModel = AppModel(MegaContent(
      sendingActions = Set.empty[Action],
      false,
      AppConfiguration(),
      Empty,
      Empty,
      Map(),
      //EditEOFault(Empty,0),
      EOCache(Empty,Map(),Map()), //Map.empty[String, EOValue],Map.empty[String, EOValue],
      previousPage = Some(d2wContext)
      )
    )
  }*/
  //case class MegaContent(menuModel: Pot[Menus], metaDatas: Pot[MetaDatas])
  val previousH = new PreviousPageHandler(zoomTo(_.content.previousPage))
  val cacheH = new EOCacheHandler(zoomTo(_.content.cache))

  override val actionHandler = composeHandlers(
    new SendingActionsHandler(zoomTo(_.content.sendingActions)),
    new AppConfigurationHandler(zoomTo(_.content.appConfiguration)),
    new BusyIndicatorHandler(zoomTo(_.content.showBusyIndicator)),
    new MenuHandler(zoomTo(_.content.menuModel)),
    new RuleResultsHandler(zoomTo(_.content.ruleResults)),
    cacheH,
    previousH
  )


}

