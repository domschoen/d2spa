package example

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate
import diode.ActionResult.ModelUpdateEffect
import d2spa.shared.{Api, EOKeyValueQualifier, EditInspectProperty, TodoItem}
import example.services.AjaxClient
import boopickle.Default._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import japgolly.scalajs.react.extra.router.RouterCtl
import example.D2SPAMain.TaskAppPage

/*import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import japgolly.scalajs.react.extra.router._*/

import d2spa.shared.{Menus, MetaDatas, EntityMetaData, QueryProperty, QueryTask, EO, StringValue}

object AppCircuit extends Circuit[AppModel] with ReactConnector[AppModel] {
  // define initial value for the application model
  override protected def initialModel = AppModel.bootingModel


  //case class MegaContent(menuModel: Pot[Menus], metaDatas: Pot[MetaDatas])

  override val actionHandler = composeHandlers(
    new MenuHandler(zoomTo(_.content.menuModel)),
    new DataHandler(zoomTo(_.content.metaDatas)),
    new EOsHandler(zoomTo(_.content.eos)),
    new EOHandler(zoomTo(_.content.eo))
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

class DataHandler[M](modelRW: ModelRW[M, MetaDatas]) extends ActionHandler(modelRW) {

  private def zoomToEntity(entityMetaData: String, rw: ModelRW[M, MetaDatas]): Option[ModelRW[M, EntityMetaData]] = {
      rw.value.entityMetaDatas.indexWhere(n => n.entityName == entityMetaData) match {
        case -1 =>
          // should not happen!
          None
        case idx =>
            Some(rw.zoomRW(_.entityMetaDatas(idx))((m, v) =>
              m.copy(entityMetaDatas = (m.entityMetaDatas.take(idx) :+ v) ++ m.entityMetaDatas.drop(idx + 1))))
      }
  }
  private def zoomToProperty(property: QueryProperty, rw: ModelRW[M, QueryTask]): Option[ModelRW[M, QueryProperty]] = {
    rw.value.displayPropertyKeys.indexWhere(n => n.key == property.key) match {
      case -1 =>
        // should not happen!
        None
      case idx =>
        Some(rw.zoomRW(_.displayPropertyKeys(idx))((m, v) =>
          m.copy(displayPropertyKeys = (m.displayPropertyKeys.take(idx) :+ v) ++ m.displayPropertyKeys.drop(idx + 1))))
    }
  }


  // handle actions
  override def handle = {
    case InitMetaData =>
      println("InitMetaData ")
      effectOnly(Effect(AjaxClient[Api].getMetaData().call().map(SetMetaData)))
    case SetMetaData(metaData) =>
      updated(metaData)

    case UpdateQueryProperty(entity, property, newEOValue) =>
      println("UpdateProperty: for entity " + entity + " property: " + property + " " + newEOValue)
      val entityWriter = zoomToEntity(entity,modelRW)
      entityWriter match {
        case Some(erw) => zoomToProperty(property, erw.zoomRW(_.queryTask)((qt, v) => qt.copy(queryTask = v))) match {
          case Some(prw) => {
            println("newEOValue " + newEOValue)
            println("prw " + prw)
            ModelUpdate(prw.updated(prw.value.copy(value = newEOValue)))
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
      updated(
        Ready(eo),
        Effect(AfterEffectRouter.setEditPageForEntity(eo.entity))
      )
    case InspectEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallInspectPage(fromTask,eo))
      )
    case NewEOPage(selectedEntity) =>
      println("edit page for entity " + selectedEntity)

      // Example of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )
      effectOnly(Effect(AjaxClient[Api].newEO(selectedEntity).call().map(EOCreated)))
    case UpdateEOValueForProperty(entity, property, newEOValue) =>
      println("Update EO Property: for entity " + entity + " property: " + property + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      val eo = value.get
      updated(Ready(value.get.copy(values = (eo.values - property.key) + (property.key -> newEOValue))))
  }
}

//       updated(value.copy(d2wContext = value.d2wContext.copy(entity = entity, task = "list")), )

class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitMenu =>
      println("InitMenu ")
      effectOnly(Effect(AjaxClient[Api].getMenus().call().map(SetMenus)))
    case SetMenus(menus) =>
      updated(Ready(menus),Effect.action(InitMetaData) )
    /*case InitMenuSelection =>
      println("Initializing Menus")
      updated(value.copy(d2wContext = value.d2wContext.copy(entity ="ChipsetSecurityType", task = "query")))*/
    case SelectMenu(selectedEntity) =>
      println("selectedEntity " + selectedEntity)

      // Example of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )

      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "query"))),
        Effect( AfterEffectRouter.setQueryPageForEntity( selectedEntity))
      )
    case SetPreviousPage(selectedEntity) =>
      val previousTask = value.get.d2wContext.previousTask
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(task = previousTask))),
        Effect( AfterEffectRouter.setPageForTaskAndEntity(previousTask, selectedEntity))
      )

    case Save(selectedEntity,eo) =>
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = "inspect"))),
        // Update the DB and dispatch the result withing UpdatedEO action
        Effect(AjaxClient[Api].updateEO(selectedEntity,eo).call().map(InspectEO("edit",_)))
      )
    case InstallQueryPage(entity) =>
      println("Query page for entity " + entity)

      effectOnly( Effect(AfterEffectRouter.setQueryPageForEntity(entity)))
    case InstallInspectPage(fromTask, eo) =>
      println("Inspect page for entity " + eo)

      // Example of a model update followed by an effect
      // An effect has to call an action. Here it is "UpdateAllTodos"
      //       updated(
      //          value.map(_.updated(item)),
      //          Effect(AjaxClient[Api].updateTodo(item).call().map(UpdateAllTodos))
      //       )
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = eo.entity, previousTask = fromTask, task = "inspect"))),
        Effect(AfterEffectRouter.setInspectPageForEntity( value.get.d2wContext.entity))
      )

    // Menu Handler --> SearchResult in EOsHandler
    case Search(selectedEntity, qualifiers) =>
      println("Search: for entity " + selectedEntity + " qualifier " + qualifiers)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)
      effectOnly(Effect(AjaxClient[Api].search(selectedEntity,qualifiers).call().map(SearchResult(selectedEntity,_))))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")),Effect(AjaxClient[Api].search(EOKeyValueQualifier("name","Sw")).call().map(SearchResult)))
        //Effect(AjaxClient[Api].deleteTodo("1").call().map(noChange)))
        //
    case ShowPage(selectedEntity, selectedTask) =>
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entity = selectedEntity, task = selectedTask))),
        Effect( AfterEffectRouter.setListPageForEntity(selectedEntity))
      )
  }
}
