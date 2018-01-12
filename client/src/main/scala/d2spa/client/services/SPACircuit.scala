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
    new QueryValuesHandler(zoomTo(_.content.queryValues)),
    new EOModelHandler(zoomTo(_.content.eomodel))
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


class EOModelHandler[M](modelRW: ModelRW[M, Pot[EOModel]]) extends ActionHandler(modelRW) {
  override def handle = {
    case FetchEOModel =>
      println("FetchEOModel")
      value match {
        case Ready(eomodel) =>
          println("FetchEOModel no change")
          noChange
        case _ =>
          println("FetchEOModel fetching")
          effectOnly(Effect(AjaxClient[Api].fetchEOModel().call().map(SetEOModel(_))))
      }

    case SetEOModel(eomodel) =>
      updated(Ready(eomodel))

  }

}

class DebugHandler[M](modelRW: ModelRW[M, Boolean]) extends ActionHandler(modelRW) {
  override def handle = {
    case SwithDebugMode =>
      updated(!value)
  }
}

class DataHandler[M](modelRW: ModelRW[M, List[EntityMetaData]]) extends ActionHandler(modelRW) {

  private def zoomToEntity(entity: EOEntity, rw: ModelRW[M, List[EntityMetaData]]): Option[ModelRW[M, EntityMetaData]] = {
    rw.value.indexWhere(n => n.entity.name.equals(entity.name)) match {
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
      value.indexWhere(n => n.entity.name.equals(entity.name)) match {
        case -1 =>
          effectOnly(Effect(AjaxClient[Api].getMetaData(entity.name).call().map(SetMetaDataForMenu(task, _))))
        case _ =>
          effectOnly(Effect(AfterEffectRouter.setPageForTaskAndEntity(task,entity.name)))
      }

    case SetMetaDataForMenu(task, entityMetaData) =>
      updated(entityMetaData :: value,Effect(AfterEffectRouter.setPageForTaskAndEntity(task, entityMetaData.entity.name)))

    case InitMetaData(entity) =>
      println("InitMetaData ")
      effectOnly(Effect(AjaxClient[Api].getMetaData(entity).call().map(SetMetaData(_))))
    case SetMetaData(entityMetaData) =>
      println("SetMetaData " + entityMetaData)
      updated(entityMetaData :: value)

    case HydrateProperty(property, keysToFire: List[String]) =>
      println("HydrateProperty " + property)
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

class EOsHandler[M](modelRW: ModelRW[M, Map[String, Seq[EO]]]) extends ActionHandler(modelRW) {

  def updatedModelForEntityNamed(eos: Seq[EO], entityName: String): Map[String, Seq[EO]] = {
    value + (entityName -> eos)
  }

  override def handle = {
    case FetchObjectsForEntity(entity) =>
      println("Fetch Entity " + entity)
      effectOnly(Effect(AjaxClient[Api].search(entity, List.empty[QueryValue]).call().map(FetchedObjectsForEntity(entity, _))))

    case FetchedObjectsForEntity(entity, eoses) =>
      println("FetchedObjectsForEntity Entity " + entity)
      //println("FetchedObjectsForEntity eos " + eos)
      updated(updatedModelForEntityNamed(eoses,entity.name))

    case SearchResult(entity, eoses) =>
      println("length " + eoses.length)
      updated(
        updatedModelForEntityNamed(eoses,entity.name),
        Effect(AfterEffectRouter.setListPageForEntity(entity.name))
      )
    case DeleteEOFromList(fromTask, eo) =>
      /*val eos = value.get
      val newEos = eos.filterNot(o => {o.id.equals(eo.id)})
      updated(Ready(newEos))*/
      effectOnly(Effect(AjaxClient[Api].deleteEO(eo).call().map( deletedEO => {
        val onError = deletedEO.validationError.isDefined
        if (onError) {
          println("Deleted EO error " + deletedEO.validationError)
          UpdateEOsForEOOnError(deletedEO)

        } else {
          println("Deleted EO action ")

          DeletedEO(deletedEO)
        }
      })))
    case DeletedEO(deletedEO) =>
      println("Deleted EO " + deletedEO)
      val entityName = deletedEO.entity.name
      val eos = value(entityName)
      val deletedEOPk = EOValueUtils.pk(deletedEO).get

      val newEos = eos.filterNot(o => {
        val pk = EOValueUtils.pk(o)
        pk.isDefined && pk.get.equals(deletedEOPk)})
      updated(updatedModelForEntityNamed(newEos,entityName))

    case UpdateEOsForEOOnError(eoOnError) =>
      val escapedHtml = Utils.escapeHtml(eoOnError.validationError.get)
      val eoWithDisplayableError = eoOnError.copy(validationError = Some(escapedHtml))
      val entityName = eoOnError.entity.name
      val eos = value(entityName)
      val deletedEOPk = EOValueUtils.pk(eoWithDisplayableError).get
      val newEos = eos.map(o => {
        val pk = EOValueUtils.pk(o)
        if (pk.isDefined && pk.get.equals(deletedEOPk)) eoWithDisplayableError else o})
      updated(updatedModelForEntityNamed(newEos,entityName))


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
    case CompleteEO(eo, missingKeys) =>
      effectOnly(Effect(AjaxClient[Api].completeEO(eo,missingKeys).call().map(RefreshEO)))
    case RefreshEO(eo) =>
      println("Refreshed EO " + eo)
      updated(Ready(eo))

    case EditEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallEditPage(fromTask,eo))
      )
    case NewEOPage(selectedEntity) =>
      updated(
        Empty,
        Effect.action(FetchMetaDataForMenu("edit",selectedEntity)) // from edit ?
      )


    case UpdateEOValueForProperty(eo, entity, property, newEOValue) =>
      println("Update EO Property: for entity " + entity + " property: " + property + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      println("EO: " + eo)

      updated(Ready(eo.copy(values = (eo.values - property.d2wContext.propertyKey) + (property.d2wContext.propertyKey -> newEOValue))))
  }
}

//       updated(value.copy(d2wContext = value.d2wContext.copy(entity = entity, task = "list")), )

class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitClient =>
      println("InitMenu ")
      if (value.isEmpty) {
        println("Api get Menus")
        effectOnly(Effect(AjaxClient[Api].getMenus().call().map(SetMenus)))
      } else
        noChange
    case InitMenuAndEO(eo, missingKeys) =>
      effectOnly(Effect(AjaxClient[Api].getMenus().call().map(menus => {
        SetMenusAndEO(menus, eo, missingKeys)
      })))

    case SetMenus(menus) =>
      println("Set Menus " + menus)
      updated(Ready(menus),Effect.action(FetchEOModel))

    case SelectMenu(selectedEntity) =>
      println("selectedEntity " + selectedEntity)
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "query"))),
        Effect.action(SetupQueryPageForEntity(selectedEntity))
      )
    case SetPreviousPage(selectedEntity) =>
      val previousTask = value.get.d2wContext.previousTask
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(task = previousTask))),
        Effect(AfterEffectRouter.setPageForTaskAndEntity(previousTask, selectedEntity.name))
      )

    case Save(selectedEntity, eo) =>
      println("SAVE " + eo)
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "inspect"))),
        // Update the DB and dispatch the result withing UpdatedEO action
        Effect(AjaxClient[Api].updateEO(eo).call().map(newEO => {
          val onError = newEO.validationError.isDefined
          if (onError) {
            EditEO("edit", newEO)

          } else {
            InspectEO("edit", newEO)
          }
        }
        ))
      )
    case NewEO(selectedEntity, eo) =>
      println("SAVE " + eo)
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "inspect"))),
        // Update the DB and dispatch the result withing UpdatedEO action
        Effect(AjaxClient[Api].newEO(selectedEntity, eo).call().map(newEO => {
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
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = eo.entity, previousTask = fromTask, task = "inspect"))),
        Effect(AfterEffectRouter.setInspectPageForEntity(value.get.d2wContext.entity.name))
      )
    case InstallEditPage(fromTask, eo) =>
      println("Edit page for entity " + eo)
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = eo.entity, previousTask = fromTask, task = "edit"))),
        Effect(AfterEffectRouter.setEditPageForEntity(value.get.d2wContext.entity.name))
      )

    case ShowPage(selectedEntity, selectedTask) =>
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = selectedTask))),
        Effect(AfterEffectRouter.setListPageForEntity(selectedEntity.name))
      )
  }

}

class QueryValuesHandler[M](modelRW: ModelRW[M, List[QueryValue]]) extends ActionHandler(modelRW) {

  override def handle = {
    // Menu Handler --> SearchResult in EOsHandler
    case Search(selectedEntity) =>
      println("Search: for entity " + selectedEntity + " with query values " + value)
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


