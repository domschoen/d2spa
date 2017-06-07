package example

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate
import diode.ActionResult.ModelUpdateEffect
import spatutorial.shared.{TodoItem, Api, EOKeyValueQualifier}
import example.services.AjaxClient
import boopickle.Default._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object AppCircuit extends Circuit[AppModel] with ReactConnector[AppModel] {
  // define initial value for the application model
  override protected def initialModel = AppModel.bootingModel

  override val actionHandler = composeHandlers(
    new MenuHandler(zoomTo(_.content.menuModel)),
    new DataHandler(zoomTo(_.content.metaDatas))
  )
}

/*
class AppModelHandler[M](modelRW: ModelRW[M, AppModel]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitAppModel =>
      updated(initialModel)
  }
}*/

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
  override def handle = {
    case SearchResult(eoses) =>
      val entity = "DTEEMI"
      println("SearchResult: for entity " + entity + " eos: " + eoses)
      val entityWriter = zoomToEntity(entity,modelRW)
      entityWriter match {
        case Some(erw) => {
          println("erw " + erw)
          val rwForListTask = erw.zoomRW(_.listTask)((m,v) => m.copy(listTask = v))
          ModelUpdateEffect(rwForListTask.updated(rwForListTask.value.copy(eos = Ready(eoses))),Effect.action(ShowPage(entity,"list")))
        }
        case None     => {
          println("no changes ")
          noChange
        }
      }
    case UpdateQueryProperty(entity, property, newEOValue) =>
      println("UpdateProperty: for entity " + entity + " property: " + property)
      val entityWriter = zoomToEntity(entity,modelRW)
      entityWriter match {
        case Some(erw) => zoomToProperty(property, erw.zoomRW(_.queryTask)((qt, v) => qt.copy(queryTask = v))) match {
          case Some(prw) => ModelUpdate(prw.updated(prw.value.copy(value = newEOValue)))
          case None     => noChange
        }
        case None     => noChange
      }
  }
}


//       updated(value.copy(d2wContext = value.d2wContext.copy(entity = entity, task = "list")), )

class MenuHandler[M](modelRW: ModelRW[M, Menus]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitMenuSelection =>
      println("Initializing Menus")
      updated(value.copy(d2wContext = value.d2wContext.copy(entity ="ChipsetSecurityType", task = "query")))
    case DickChange(nosay) =>
      println("DickChange" + nosay)
      noChange
    case SelectMenu(selectedEntity) =>
      println("selectedEntity " + selectedEntity)
      updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "query")))
    case Search(selectedEntity, qualifiers) =>
      println("Search: for entity " + selectedEntity)
      effectOnly(Effect(AjaxClient[Api].search(qualifiers.head).call().map(SearchResult)))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")))
      //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")),Effect(AjaxClient[Api].search(EOKeyValueQualifier("name","Sw")).call().map(SearchResult)))
        //Effect(AjaxClient[Api].deleteTodo("1").call().map(noChange)))
        //
    case ShowPage(selectedEntity, selectedTask) =>
      updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = selectedTask)))
  }
}
