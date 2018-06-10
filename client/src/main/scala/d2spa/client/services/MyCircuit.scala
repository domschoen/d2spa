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


  //case class MegaContent(menuModel: Pot[Menus], metaDatas: Pot[MetaDatas])

  override val actionHandler = composeHandlers(
    new DebugConfigurationHandler(zoomTo(_.content.debugConfiguration)),
    new BusyIndicatorHandler(zoomTo(_.content.showBusyIndicator)),
    new MenuHandler(zoomTo(_.content.menuModel)),
    new RuleResultsHandler(zoomTo(_.content.ruleResults)),
    new EOCacheHandler(zoomTo(_.content.cache)),
    new EOModelHandler(zoomTo(_.content.eomodel)),
    new PreviousPageHandler(zoomTo(_.content.previousPage))
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

    case  InspectEO (fromTask, eo, isOneRecord) =>
      log.debug("PreviousPageHandler | InspectEO from: " + fromTask)
      val d2wContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.inspect), eo = Some(eo))
      effectOnly(Effect.action(RegisterPreviousPage(d2wContext)))


    case  InitMetaDataForList (entityName) =>
      log.debug("PreviousPageHandler | InitMetaDataForList for: " + entityName)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list))
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)

      effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaData(d2wContext,_))))

    case InitMetaData(entityName) =>
      log.debug("PreviousPageHandler | InitMetaData for Query page: " + entityName)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.query))
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
      updated(
        // change context to inspect
        Some(d2wContext),
        Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaData(d2wContext,_))))

    case RegisterPreviousPage(d2WContext) =>
      val  stack = stackD2WContext(d2WContext)
      log.debug("PreviousPageHandler | RegisterPreviousPage for d2wContext: " + stack)
      updated(
        // change context to inspect
        Some(stack),
        Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(d2WContext))
      )

    case SetPreviousPage =>
      log.debug("PreviousPageHandler | SetPreviousPage to: " + value)
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
      log.debug("PreviousPageHandler | UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue)
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues + (queryValue.key -> queryValue)
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case ClearQueryProperty(entityName, propertyName) =>
      log.debug("PreviousPageHandler | ClearQueryProperty: for entity " + entityName + " and property " + propertyName)
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues - propertyName
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case Search(entityName) =>
      log.debug("PreviousPageHandler | Search | " + entityName + " | value: " + value)
      val fs: EOFetchSpecification = value match {
        case Some(d2wContext) =>
          val qualifierOpt = QueryValue.qualifierFromQueryValues(d2wContext.queryValues.values.toList)
          log.debug("SPACircuit | Search(" + entityName + ") | qualifierOpt: " + qualifierOpt)

          qualifierOpt match {
            // TODO manage sort ordering
            case Some(qualifier) => EOQualifiedFetch(entityName,qualifier,List())
            case _ =>   EOFetchAll(entityName)
          }
        case _ => EOFetchAll(entityName)  // shouldn't come here because the query page initialized it
      }
      log.debug("PreviousPageHandler | Search | " + entityName + " query with fs " + fs)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)

      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list), dataRep = Some(DataRep(Some(fs))))
      val  stack = stackD2WContext(d2wContext)
      log.debug("PreviousPageHandler | Search | Register Previous " + stack)

      val effect = fs match {
        case fa: EOFetchAll => Effect(AjaxClient[Api].searchAll(fa).call().map(SearchResult(entityName, _)))
        case fq: EOQualifiedFetch => Effect(AjaxClient[Api].search(fq).call().map(SearchResult(entityName, _)))
      }

      updated(
        // change context to inspect
        Some(stack),
        effect
      )

  }
}

