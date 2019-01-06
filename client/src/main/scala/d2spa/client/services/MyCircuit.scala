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

class PreviousPageHandler[M](modelRW: ModelRW[M, Option[D2WContext]]) extends ActionHandler(modelRW) {

  def stackD2WContext(d2WContext: D2WContext) : D2WContext = {
    value match {
      case Some(currentPage) => {
        d2WContext.copy(previousTask = value)
      }
      case _ => d2WContext
    }
  }

  override def handle = {
    case InitAppSpecificClient(ffd2wContext) =>
      log.finest("PreviousPageHandler | InitAppSpecificClient: " + ffd2wContext)
      val d2wContext = D2WContextUtils.convertFullFledgedToD2WContext(ffd2wContext)
      val allowedTasks = Set(TaskDefine.edit, TaskDefine.inspect)
      val task = ffd2wContext.task.get
      if (allowedTasks.contains(task)) {
        effectOnly(Effect.action(PrepareEODisplay(d2wContext)))
      } else {
        effectOnly(Effect.action(GetMetaDataForSetPage(d2wContext)))
      }



    case ShowResults(fs) =>
      log.finest("PreviousPageHandler | ShowResults: " + fs)
      val entityName = EOFetchSpecification.entityName(fs)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list), dataRep = Some(DataRep(Some(fs))))
      effectOnly(Effect.action(RegisterPreviousPageAndSetPage(d2wContext)))


    case  InitMetaDataForList (entityName) =>
      log.finest("PreviousPageHandler | InitMetaDataForList for: " + entityName)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list))
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)

      SPAMain.socket.send(WebSocketMessages.GetMetaData(fullFledged))
      noChange

    case InitMetaData(d2wContext) =>
      log.finest("PreviousPageHandler | InitMetaData: " + d2wContext.entityName)
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)

      SPAMain.socket.send(WebSocketMessages.GetMetaData(fullFledged)) // reply with RuleResults and then action SetJustRuleResults
      noChange

    case UpdateCurrentContextWithEO(eo) =>
      val newContext = value.get.copy(eo = Some(eo))
      updated(Some(newContext))


    case RegisterPreviousPageAndSetPage(d2wContext) =>
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
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues + (queryValue.key -> queryValue)
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case ClearQueryProperty(entityName, propertyName) =>
      log.finest("PreviousPageHandler | ClearQueryProperty: for entity " + entityName + " and property " + propertyName)
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues - propertyName
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case PrepareSearchForServer(d2wContext, isMetaDataFetched) =>
      val entityName = d2wContext.entityName.get
      log.finest("PreviousPageHandler | Search | " + entityName + " | value: " + value)
      val fs: EOFetchSpecification = value match {
        case Some(d2wContext) =>
          val qualifierOpt = QueryValue.qualifierFromQueryValues(d2wContext.queryValues.values.toList)
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

      SPAMain.socket.send(WebSocketMessages.Search(fs, isMetaDataFetched))
      noChange

      //updated(
        // change context to inspect
      //  Some(stack)
      //)

  }
}

