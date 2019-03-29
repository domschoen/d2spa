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
    new PreviousPageHandler2(zoomTo(_.content.previousPage)),
    cacheH,
    previousH
  )


}

class PreviousPageHandler2[M](modelRW: ModelRW[M, Option[PageContext]]) extends ActionHandler(modelRW) {

  def stackD2WContext(d2WContext: PageContext) : PageContext = {
    value match {
      case Some(currentPage) => {
        d2WContext.copy(previousTask = value)
      }
      case _ => d2WContext
    }
  }

  override def handle = {
    case InitAppSpecificClient(pageContext) =>
      val d2wContext = pageContext.d2wContext
      log.finest("PreviousPageHandler | InitAppSpecificClient: " + d2wContext)
      val allowedTasks = Set(TaskDefine.edit, TaskDefine.inspect)
      val task = d2wContext.task.get
      if (allowedTasks.contains(task)) {
        effectOnly(Effect.action(PrepareEODisplay(pageContext)))
      } else {
        effectOnly(Effect.action(GetMetaDataForSetPage(pageContext)))
      }


    case ShowResults(fs) =>
      log.finest("PreviousPageHandler | ShowResults: " + fs)
      val entityName = EOFetchSpecification.entityName(fs)
      val pageContext = PageContext (
        dataRep = Some(DataRep(Some(fs))),
        d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list)))
      effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContext)))


    case  SendRuleRequest (ruleRequest) =>
      log.finest("PreviousPageHandler | SendRuleRequest | ruleRequest: " + ruleRequest)
      WebSocketClient.send(WebSocketMessages.ExecuteRuleRequest(ruleRequest))
      noChange


    case UpdateCurrentContextWithEO(eo) =>
      val d2wContext = value.get.d2wContext
      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      val pageContextUpdated = pageContext.copy(eo = Some(eo))
      updated(Some(pageContextUpdated))


    case RegisterPreviousPageAndSetPage(pageContext) =>
      val d2wContext = pageContext.d2wContext

      log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage: " + d2wContext.entityName)
      val newEOToBeRemovedFromCache = value match {
        case Some(currentPage) =>
          val currentPageD2WContext = currentPage.d2wContext
          if (currentPageD2WContext.task.get.equals(TaskDefine.edit)) {
            val eoOpt = currentPage.eo
            eoOpt match {
              case Some(eo) =>
                val isNewEO = EOValue.isNewEO(eo)
                log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage | eo " + eo)
                log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage | isNewEO " + isNewEO)
                if (isNewEO) {
                  Some(eo)
                } else None
              case None =>
                None
            }
          } else None
        case None => None
      }
      newEOToBeRemovedFromCache match {
        case Some(eo) =>
          effectOnly(Effect.action(RegisterPreviousPageAndSetPageRemoveMemEO(pageContext, eo)))

        case None =>
          effectOnly(Effect.action(RegisterPreviousPageAndSetPagePure(pageContext)))
      }


    case RegisterPreviousPageAndSetPagePure(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage for d2wContext: " + stack)

      updated(Some(stack),Effect.action(SetPage(stack)))



    case RegisterPreviousPage(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | RegisterPreviousPage for d2wContext: " + stack)

      updated(Some(stack))

    case PrepareEditPage(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | PrepareEditPage for d2wContext: " + stack)

      updated(Some(stack),Effect.action(PrepareEODisplay(stack)))

    case SetPage(d2WContext) =>
      log.finest("PreviousPageHandler | SetPage for d2wContext: " + d2WContext)
      effectOnly(
        Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(d2WContext))
      )


    case SetPreviousPage =>
      log.finest("PreviousPageHandler | SetPreviousPage to: " + value)
      value match {
        case Some(currentPage) => {
          currentPage.previousTask match {
            case Some(previousPage) =>
              updated(
                Some(previousPage),
                Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(previousPage))
              )
            case _ => noChange
          }
        }
        case _ => noChange
      }

    case UpdateQueryProperty(entityName, queryValue) =>
      log.finest("PreviousPageHandler | UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue)
      val pageContext = value.get
      val newD2wContext = D2WContextUtils.pageContextUpdatedWithQueryValue(pageContext,queryValue)
      updated(Some(newD2wContext))

    case ClearQueryProperty(entityName, propertyName, operator) =>
      log.finest("PreviousPageHandler | ClearQueryProperty: for entity " + entityName + " and property " + propertyName)
      val pageContext = value.get
      val newPageContext = D2WContextUtils.updatePageContextByClearingKeyForOperator(pageContext,propertyName, operator)
      updated(Some(newPageContext))

    case SearchAction(entityName) =>
      log.finest("PreviousPageHandler | PrepareSearchForServer | entityName: " + entityName)
      val fs: EOFetchSpecification = value match {
        case Some(d2wContext) =>
          val qualifierOpt = QueryValue.qualifierFromQueryValues(d2wContext.queryValues)
          log.finest("SPACircuit | Search(" + entityName + ") | qualifierOpt: " + qualifierOpt)

          qualifierOpt match {
            // TODO manage sort ordering
            case Some(qualifier) => EOQualifiedFetch(entityName,qualifier,List())
            case _ =>   EOFetchAll(entityName)
          }
        case _ => EOFetchAll(entityName)  // shouldn't come here because the query page initialized it
      }
      log.finest("PreviousPageHandler | Search | " + entityName + " query with fs " + fs)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)

      //val  stack = stackD2WContext(d2wContext)
      //log.finest("PreviousPageHandler | Search | Register Previous " + stack)

      effectOnly(Effect.action(SearchHydration(fs)))

    //updated(
    // change context to inspect
    //  Some(stack)
    //)

  }
}

