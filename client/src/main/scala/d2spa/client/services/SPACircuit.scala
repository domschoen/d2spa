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

  private def zoomToEntity(entityName: String, rw: ModelRW[M, List[EntityMetaData]]): Option[ModelRW[M, EntityMetaData]] = {
    rw.value.indexWhere(n => n.entity.name.equals(entityName)) match {
      case -1 =>
        // should not happen!
        None
      case idx =>
        Some(rw.zoomRW(_(idx))((m, v) =>
          (m.take(idx) :+ v) ++ m.drop(idx + 1)))
    }
  }
  private def zoomToProperty(property: PropertyMetaInfo, rw: ModelRW[M, Task]): Option[ModelRW[M, PropertyMetaInfo]] = {
    rw.value.displayPropertyKeys.indexWhere(n => n.name == property.name) match {
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

  // case class RuleResult(rhs: D2WContextFullFledged, key: String, value: RuleValue)

  def ruleResultsWith(base: List[RuleResult], addOn: List[RuleResult]): List[RuleResult] = {
    println("Mix base " + base)
    println("+ addOn  " + addOn)

    val baseMap = base.map(x => ((x.rhs, x.key),x)).toMap
    val addOnMap = addOn.map(x => ((x.rhs, x.key),x)).toMap
    val mixMap = baseMap ++ addOnMap
    val result = mixMap.values.toList
    println("result   " + result)
    result
  }


  // handle actions
  override def handle = {
    case FetchMetaDataForMenu(task, entityName) =>
      println("InitMetaData ")
      value.indexWhere(n => n.entity.name.equals(entityName)) match {
        case -1 =>
          effectOnly(Effect(AjaxClient[Api].getMetaData(entityName).call().map(SetMetaDataForMenu(task, _))))
        case _ =>
          effectOnly(Effect(AfterEffectRouter.setPageForTaskAndEntity(task,entityName)))
      }

    case SetMetaDataForMenu(task, entityMetaData) =>
      updated(entityMetaData :: value,Effect(AfterEffectRouter.setPageForTaskAndEntity(task, entityMetaData.entity.name)))

    case InitMetaData(entity) =>
      println("InitMetaData ")
      effectOnly(Effect(AjaxClient[Api].getMetaData(entity).call().map(SetMetaData(_))))
    case SetMetaData(entityMetaData) =>
      println("SetMetaData " + entityMetaData)
      updated(entityMetaData :: value)


    case FireActions(property: PropertyMetaInfo, actions: List[D2WAction]) =>
      println("FireActions " )
      // take first actions and call FireActions again with the rest
      val fireAction = actions.head
      val remainingActions = actions.tail
      fireAction match {
        case FireRule(rhs, key) =>
          println("Fire Rule " + key + " context: " + rhs)
          // convert any rhs depending on previous results
          val newRhs = if (rhs.pageConfiguration.isDefined && rhs.pageConfiguration.get.isLeft) {
            rhs.copy(pageConfiguration = Some(Right("real value")))
          } else rhs
          effectOnly(Effect(AjaxClient[Api].fireRule(newRhs,key).call().map(rr => SetRuleResults(List(rr), property, remainingActions))))

        case Hydration(drySubstrate,  wateringScope) =>
          // get displayPropertyKeys from previous rule results
          val fireRule = wateringScope.fireRule.get
          val ruleKey = fireRule.key
          val ruleRhs = RuleUtils.convertFullFledgedToD2WContext(fireRule.rhs)
          val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(property.ruleResults,ruleRhs,ruleKey)
          log.debug("Hydration with scope defined by rule " + ruleResultOpt)
          ruleResultOpt match {
            case Some(RuleResult(rhs,key,value)) => {

              val ruleValue = property.ruleResults
              val missingKeys: Set[String] = ruleKey match {
                case RuleKeys.keyWhenRelationship =>
                  Set(value.stringV.get)
                  //Set(value)
                case RuleKeys.displayPropertyKeys =>
                  value.stringsV.toSet
                  //Set(value)
              }
              drySubstrate match {
                case DrySubstrate(_ , Some(eo), _) =>
                  // completeEO ends up with a MegaContent eo update
                  effectOnly(Effect(AjaxClient[Api].completeEO(eo,missingKeys).call().map(RefreshEO(_, property, remainingActions))))
                case DrySubstrate(Some(eorefsdef), _ , _) =>
                  eorefsdef match {
                    case EORefsDefinition(Some(eoakp)) =>
                      val eovalueOpt = EOValueUtils.valueForKey(eoakp.eo, eoakp.keyPath)
                      eovalueOpt match {
                        case Some(eovalue) =>
                          val eoRefs = eovalue.eosV
                          effectOnly(Effect(AjaxClient[Api].hydrateEORefs(eoRefs,missingKeys).call().map(FetchedObjectsForEntity(_, property, remainingActions))))

                        case _ => effectOnly(Effect.action(FireActions(property,remainingActions))) // we skip the action ....
                      }
                    case _ => effectOnly(Effect.action(FireActions(property,remainingActions))) // we skip the action ....
                  }
                case DrySubstrate(_ , _ , Some(fs)) =>
                  effectOnly(Effect(AjaxClient[Api].search(fs.entityName, List.empty[QueryValue]).call().map(FetchedObjectsForEntity(_ , property, remainingActions))))

                case _ =>  effectOnly(Effect.action(FireActions(property,remainingActions))) // we skip the action ....
              }
            }
            case _ =>  effectOnly(Effect.action(FireActions(property,remainingActions))) // we skip the action ....
          }
      }




    // many rules
    case SetRuleResults(ruleResults, property, actions: List[D2WAction]) =>
      println("Rule Results " + ruleResults)
      //val d2wContext = property.d2wContext
      //val entity = d2wContext.entity
      //val task = d2wContext.task
      val propertyKey = property.name
      val task = property.task



      // rule results are stored in EntityMetaData -> Task -> property
      val entityWriter = zoomToEntity(property.entityName,modelRW)
      entityWriter match {
        case Some(erw) => zoomToTask(task, erw) match {
          case Some(trw) => {
            zoomToProperty(property, trw) match {
              case Some(propWriter) => {
                ModelUpdateEffect(propWriter.updated(propWriter.value.copy(ruleResults = ruleResultsWith(propWriter.value.ruleResults,ruleResults))),
                Effect.action(FireActions(property, actions)))
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

// hydrated destination EOs are simply stored in MegaContent eos
class EOsHandler[M](modelRW: ModelRW[M, Map[String, Map[Int,EO]]]) extends ActionHandler(modelRW) {

  // eo chache is stored as:
  // Map of entityName -> Map of id -> eo
  // eos: Map[String, Map[Int,EO]],

  def updatedModelForEntityNamed(eos: Seq[EO]): Map[String, Map[Int,EO]] = {
    if (eos.isEmpty) {
      value
    } else {
      val result : Map[String, Map[Int,EO]] = subPart(eos)
      println("storing result as :" + result)
      result
    }
  }


  def subPart(eos: Seq[EO]) = {
    val anyHead = eos.headOption
    anyHead match {
      case Some(head) =>
        val entity = head.entity
        val pkAttributeName = entity.pkAttributeName
        val entityName = entity.name
        val entityMap = if (value.contains(entityName)) value(entity.name) else Map.empty[Int,EO]

        // we create a Map with eo and id
        val refreshedEOs = eos.map(eo => {
          val pkOpt = EOValueUtils.pk(eo)
          pkOpt match {
            case Some(pk) => Some((pk,eo))
            case _ => None
          }
        }).flatten.toMap

        // work with new and eo to be updated
        val refreshedPks = refreshedEOs.keySet
        val existingPks = entityMap.keySet

        val newPks = refreshedPks -- existingPks

        // Iterate on eos
        val newAndUpdateMap = refreshedPks.map(id => {
          val refreshedEO = refreshedEOs(id)

          // 2 cases:
          // new
          if (newPks.contains(id)) {
            (id, refreshedEO)
          } else {
            // existing -> to be updated
            // Complete EOs !
            val existingEO = entityMap(id)
            (id, EOValueUtils.completeEoWithEo(existingEO,refreshedEO))
          }
        }).toMap
        val newEntityMap = entityMap ++ newAndUpdateMap

        value + (entity.name -> newEntityMap)
      case _ => Map.empty[String, Map[Int,EO]]
    }
  }

  override def handle = {

    case FetchedObjectsForEntity(eoses, property, actions) =>
      println("FetchedObjectsForEntity property " + property)
      //println("FetchedObjectsForEntity eos " + eos)
      updated(
        updatedModelForEntityNamed(eoses),
        Effect.action(FireActions(property, actions))
      )

    case SearchResult(entity, eoses) =>
      println("length " + eoses.length)
      updated(
        updatedModelForEntityNamed(eoses),
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
      val eoPk = EOValueUtils.pk(deletedEO).get

      val entityMap = value(entityName)
      val newEntityMap = entityMap - eoPk
      val newValue = value + (entityName -> newEntityMap)
      updated(newValue)


    // set error on eo
    case UpdateEOsForEOOnError(eoOnError) =>
      val escapedHtml = Utils.escapeHtml(eoOnError.validationError.get)
      val eoWithDisplayableError = eoOnError.copy(validationError = Some(escapedHtml))
      val entityName = eoOnError.entity.name
      val eos = value(entityName)
      val eoPk = EOValueUtils.pk(eoWithDisplayableError).get
      val entityMap = value(entityName)
      val newEntityMap = entityMap + (eoPk -> eoWithDisplayableError)
      val newValue = value + (entityName -> newEntityMap)
      updated(newValue)


}

}
class EOHandler[M](modelRW: ModelRW[M, Pot[EO]]) extends ActionHandler(modelRW) {

  override def handle = {
    case EOCreated(eo) =>
      println("eo created " + eo)
      updated(
        Ready(eo),
        Effect.action(FetchMetaDataForMenu("edit", eo.entity.name))
      )
    case InspectEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallInspectPage(fromTask,eo))
      )
    case RefreshEO(eo, property, actions) =>
      println("Refreshed EO " + eo)
      updated(
        Ready(eo),
        Effect.action(FireActions(property,actions))
      )

    case EditEO(fromTask, eo) =>
      updated(
        Ready(eo),
        Effect.action(InstallEditPage(fromTask,eo))
      )
    case NewEOPage(selectedEntityName) =>
      updated(
        Empty,
        Effect.action(FetchMetaDataForMenu("edit", selectedEntityName)) // from edit ?
      )


    case UpdateEOValueForProperty(eo, entityName, property, newEOValue) =>
      println("Update EO Property: for entity " + entityName + " property: " + property + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      println("EO: " + eo)
      val propertyName = property.name
      updated(Ready(eo.copy(values = (eo.values - propertyName) + (propertyName -> newEOValue))))
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

    case SelectMenu(entityName) =>
      println("selectedEntity " + entityName)
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(entityName), task = Some("query")))),
        Effect.action(SetupQueryPageForEntity(entityName))
      )
    case SetPreviousPage(selectedEntity) =>
      val previousTask = value.get.d2wContext.previousTask
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(task = Some(previousTask.get)))),
        Effect(AfterEffectRouter.setPageForTaskAndEntity(previousTask.get, selectedEntity.name))
      )

    case Save(selectedEntityName, eo) =>
      println("SAVE " + eo)
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(selectedEntityName), task = Some("inspect")))),
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
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(selectedEntity.name), task = Some("inspect")))),
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
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(
          entityName = Some(eo.entity.name),
          previousTask = Some(fromTask), task = Some("inspect")))),
        Effect(AfterEffectRouter.setInspectPageForEntity(value.get.d2wContext.entityName.get))
      )
    case InstallEditPage(fromTask, eo) =>
      println("Edit page for entity " + eo)
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(eo.entity.name), previousTask = Some(fromTask), task = Some("edit")))),
        Effect(AfterEffectRouter.setEditPageForEntity(value.get.d2wContext.entityName.get))
      )

    case ShowPage(selectedEntity, selectedTask) =>
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(selectedEntity.name), task = Some(selectedTask)))),
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
      effectOnly(Effect(AjaxClient[Api].search(selectedEntity.name, value).call().map(SearchResult(selectedEntity, _))))
    //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")))
    //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")),Effect(AjaxClient[Api].search(EOKeyValueQualifier("name","Sw")).call().map(SearchResult)))
    //Effect(AjaxClient[Api].deleteTodo("1").call().map(noChange)))
    //

    case SetupQueryPageForEntity(selectedEntityName) =>
      updated(
        List(),
        Effect.action(FetchMetaDataForMenu("query", selectedEntityName))
      )

    case UpdateQueryProperty(entityName, queryValue) =>
      updated(queryValue :: value)
  }
}


