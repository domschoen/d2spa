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

/*import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import japgolly.scalajs.react.extra.router._*/

import d2spa.client.AppModel

import d2spa.shared.{Menus, EntityMetaData, PropertyMetaInfo, Task, EO, EOValue}

// The First page displayed is the Query page. When the query page is mounted, it calls the server for the entities to be displayed.
// This is done with an action "InitMetaData" which trigger "" on the server "getMetaData"
// When data get back from server, SetMetaData is called, followed by an action "InitMenu" which goes to server and trigger a Menu.json on D2SPAServer
// D2SPAServer return a list of entities
//
object SPACircuit extends Circuit[AppModel] with ReactConnector[AppModel] {
  // define initial value for the application model
  override protected def initialModel = AppModel.bootingModel


  //case class MegaContent(menuModel: Pot[Menus], metaDatas: Pot[MetaDatas])

  override val actionHandler = composeHandlers(
    new DebugHandler(zoomTo(_.content.isDebugMode)),
    new MenuHandler(zoomTo(_.content.menuModel)),
    new DataHandler(zoomTo(_.content.entityMetaDatas)),
    new EOsHandler(zoomTo(_.content.eos)),
    new EOHandler(zoomTo(_.content.eo)),
    new QueryValuesHandler(zoomTo(_.content.queryValues))
    //new MegaDataHandler(zoomTo(_.content))
  )


}

/*
class AppModelHandler[M](modelRW: ModelRW[M, AppModel]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitAppModel =>
      updated(initialModel)
  }
}*/


/*
class MegaDataHandler[M](modelRW: ModelRW[M, MegaContent]) extends ActionHandler(modelRW) {
  override def handle = {
    case BootingModel =>
      effectOnly(Effect(AjaxClient[Api].getBootingModel().call().map(x => SetBootingModel(MegaContent(Ready(x.menuModel),x.metaDatas)))))
    case SetBootingModel(megaModel) =>
      updated(megaModel)
  }
}
*/

class DebugHandler[M](modelRW: ModelRW[M, Boolean]) extends ActionHandler(modelRW) {
  override def handle = {
    case SwithDebugMode =>
      updated(!value)
  }
}

class DataHandler[M](modelRW: ModelRW[M, List[EntityMetaData]]) extends ActionHandler(modelRW) {

  private def zoomToEntity(entityMetaData: String, rw: ModelRW[M, List[EntityMetaData]]): Option[ModelRW[M, EntityMetaData]] = {
    rw.value.indexWhere(n => n.entityName == entityMetaData) match {
      case -1 =>
        // should not happen!
        None
      case idx =>
        Some(rw.zoomRW(_(idx))((m, v) =>
          (m.take(idx) :+ v) ++ m.drop(idx + 1)))
    }
  }
  private def zoomToProperty(property: PropertyMetaInfo, rw: ModelRW[M, Task]): Option[ModelRW[M, PropertyMetaInfo]] = {
    rw.value.displayPropertyKeys.indexWhere(n => n.d2wContext.propertyKey == property.d2wContext.propertyKey) match {
      case -1 =>
        // should not happen!
        None
      case idx =>
        Some(rw.zoomRW(_.displayPropertyKeys(idx))((m, v) =>
          m.copy(displayPropertyKeys = (m.displayPropertyKeys.take(idx) :+ v) ++ m.displayPropertyKeys.drop(idx + 1))))
    }
  }
  private def zoomToTask(task: String, rw: ModelRW[M, EntityMetaData]): Option[ModelRW[M, Task]] = {
    if (task.equals(TaskDefine.edit)) {
      Some(rw.zoomRW(_.editTask) ((m, v) =>
        m.copy(editTask = v)))
    } else if (task.equals(TaskDefine.list)) {
      Some(rw.zoomRW(_.listTask)((m, v) =>
        m.copy(listTask = v)))
    } else if (task.equals(TaskDefine.inspect)) {
      Some(rw.zoomRW(_.inspectTask)((m, v) =>
        m.copy(inspectTask = v)))
    } else if (task.equals(TaskDefine.query)) {
      Some(rw.zoomRW(_.queryTask)((m, v) =>
        m.copy(queryTask = v)))
    } else {
      Some(rw.zoomRW(_.queryTask)((m, v) =>
        m.copy(queryTask = v)))
    }
  }

  def ruleResultsWith(base: List[RuleResult], addOn: List[RuleResult]): List[RuleResult] = {
    val baseMap = base.map(x => (x.key,x.eovalue)).toMap
    val addOnMap = addOn.map(x => (x.key,x.eovalue)).toMap
    val mixMap = baseMap ++ addOnMap
    mixMap.map(x => {RuleResult(x._1,x._2)}).toList
  }

  // handle actions
  override def handle = {
    case FetchMetaDataForMenu(task, entity) =>
      println("InitMetaData ")
      value.indexWhere(n => n.entityName == entity) match {
        case -1 =>
          effectOnly(Effect(AjaxClient[Api].getMetaData(entity).call().map(SetMetaDataForMenu(task, entity, _))))
        case _ =>
          effectOnly(Effect(AfterEffectRouter.setPageForTaskAndEntity(task,entity)))
      }

    case SetMetaDataForMenu(task, entity, entityMetaData) =>
      updated(entityMetaData :: value,Effect(AfterEffectRouter.setPageForTaskAndEntity(task, entity)))

    case InitMetaData(entity: String) =>
      println("InitMetaData ")
      effectOnly(Effect(AjaxClient[Api].getMetaData(entity).call().map(SetMetaData(entity, _))))
    case SetMetaData(entity: String, entityMetaData) =>
      println("SetMetaData " + entityMetaData)
      updated(entityMetaData :: value,Effect.action(InitMenu))

    case HydrateProperty(property, keysToFire: List[String]) =>
      effectOnly(Effect(AjaxClient[Api].fireRules(property.d2wContext,keysToFire).call().map(SetRuleResults(property,_))))

    case SetRuleResults(property, ruleResultByKey) =>
      println("Rule Results " + ruleResultByKey)
      val d2wContext = property.d2wContext
      val entity = d2wContext.entity
      val task = d2wContext.task
      val propertyKey = d2wContext.propertyKey
      val entityWriter = zoomToEntity(entity,modelRW)
      entityWriter match {
        case Some(erw) => zoomToTask(task, erw) match {
          case Some(trw) => {
            zoomToProperty(property, trw) match {
              case Some(prw) => {
                println("newEOValue " + ruleResultByKey)
                println("prw " + prw)
                ModelUpdate(prw.updated(prw.value.copy(ruleKeyValues = ruleResultsWith(prw.value.ruleKeyValues,ruleResultByKey))))
              }
              case None => noChange
            }
          }
          case None     => noChange
        }
        case None     => noChange
      }

  }
}

class EOsHandler[M](modelRW: ModelRW[M, Pot[Seq[EO]]]) extends ActionHandler(modelRW) {

  override def handle = {
    case SearchResult(entity, eoses) =>
      println("length " + eoses.length)
      updated(
        Ready(eoses),
        Effect(AfterEffectRouter.setListPageForEntity(entity))
      )
  }

}
class EOHandler[M](modelRW: ModelRW[M, Pot[EO]]) extends ActionHandler(modelRW) {

  override def handle = {
    case EOCreated(eo) =>
      println("eo created " + eo)
      updated(
        Ready(eo),
        Effect.action(FetchMetaDataForMenu("edit", eo.entity))
      )
    case InspectEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallInspectPage(fromTask,eo))
      )

    case EditEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallEditPage(fromTask,eo))
      )

    case UpdateEOValueForProperty(entity, property, newEOValue) =>
      println("Update EO Property: for entity " + entity + " property: " + property + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      val eo = value.get
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      println("EO: " + eo)

      updated(Ready(value.get.copy(values = (eo.values - property.d2wContext.propertyKey) + (property.d2wContext.propertyKey -> newEOValue))))
  }
}

//       updated(value.copy(d2wContext = value.d2wContext.copy(entity = entity, task = "list")), )

class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitMenu =>
      println("InitMenu ")
      if (value.isEmpty) {
        println("Api get Menus")
        effectOnly(Effect(AjaxClient[Api].getMenus().call().map(SetMenus)))
      } else
        noChange
    case SetMenus(menus) =>
      println("Set Menus " + menus)
      updated(Ready(menus)) // ,Effect.action(InitMetaData)
    /*case InitMenuSelection =>
      println("Initializing Menus")
      updated(value.copy(d2wContext = value.d2wContext.copy(entity ="ChipsetSecurityType", task = "query")))*/
    case NewEOPage(selectedEntity) =>
      println("edit page for entity " + selectedEntity)

      // d2spa.client of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "edit"))),
        Effect(AjaxClient[Api].newEO(selectedEntity).call().map(EOCreated))
      )
    case SelectMenu(selectedEntity) =>
      println("selectedEntity " + selectedEntity)

      // d2spa.client of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )

      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "query"))),
        Effect.action(SetupQueryPageForEntity(selectedEntity))
      )
    case SetPreviousPage(selectedEntity) =>
      val previousTask = value.get.d2wContext.previousTask
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(task = previousTask))),
        Effect(AfterEffectRouter.setPageForTaskAndEntity(previousTask, selectedEntity))
      )

    case Save(selectedEntity, eo) =>
      println("SAVE " + eo)
        updated(
          // change context to inspect
          Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "inspect"))),
          // Update the DB and dispatch the result withing UpdatedEO action
          Effect(AjaxClient[Api].updateEO(selectedEntity, eo).call().map( newEO => {
            val onError = newEO.validationError.isDefined
            if (onError) {
              EditEO("edit", newEO)

            } else {
              InspectEO("edit", newEO)
            }
          }
          ))
        )

    case InstallInspectPage(fromTask, eo) =>
      println("Inspect page for entity " + eo)

      // d2spa.client of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = eo.entity, previousTask = fromTask, task = "inspect"))),
        Effect(AfterEffectRouter.setInspectPageForEntity(value.get.d2wContext.entity))
      )

    case ShowPage(selectedEntity, selectedTask) =>
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = selectedTask))),
        Effect(AfterEffectRouter.setListPageForEntity(selectedEntity))
      )
  }
}

class QueryValuesHandler[M](modelRW: ModelRW[M, List[QueryValue]]) extends ActionHandler(modelRW) {

    override def handle = {
      // Menu Handler --> SearchResult in EOsHandler
      case Search(selectedEntity) =>
        println("Search: for entity " + selectedEntity)
        // Call the server to get the result +  then execute action Search Result (see above datahandler)
        effectOnly(Effect(AjaxClient[Api].search(selectedEntity, value).call().map(SearchResult(selectedEntity, _))))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")),Effect(AjaxClient[Api].search(EOKeyValueQualifier("name","Sw")).call().map(SearchResult)))
      //Effect(AjaxClient[Api].deleteTodo("1").call().map(noChange)))
      //

      case SetupQueryPageForEntity(selectedEntity) =>
        updated(
          List(),
          Effect.action(FetchMetaDataForMenu("query", selectedEntity))
        )

      case UpdateQueryProperty(entity, queryValue) =>
        updated(queryValue :: value)
    }
}


