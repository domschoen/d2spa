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
    new EOHandler(zoomTo(_.content.eo)),
    new EOCacheHandler(zoomTo(_.content.cache)),
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
      log.debug("FetchEOModel")
      value match {
        case Ready(eomodel) =>
          log.debug("FetchEOModel no change")
          noChange
        case _ =>
          log.debug("FetchEOModel fetching")
          effectOnly(Effect(AjaxClient[Api].fetchEOModel().call().map(SetEOModel(_))))
      }

    case SetEOModel(eomodel) =>
      updated(Ready(eomodel))


    case NewEOWithEntityName(selectedEntityName, property, actions) =>
      log.debug("NewEOPage: " + selectedEntityName)
      // Create the EO and set it in the cache
      effectOnly(
        Effect.action(NewEOWithEOModel(value.get, selectedEntityName, property, actions)) // from edit ?
      )


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
    log.debug("Mix base " + base)
    log.debug("+ addOn  " + addOn)

    val baseMap = base.map(x => ((x.rhs, x.key),x)).toMap
    val addOnMap = addOn.map(x => ((x.rhs, x.key),x)).toMap
    val mixMap = baseMap ++ addOnMap
    val result = mixMap.values.toList
    log.debug("result   " + result)
    result
  }


  // handle actions
  override def handle = {
    case SetPageForTaskAndEntity(task, entityName, pk) =>
      log.debug("InitMetaData ")
      value.indexWhere(n => n.entity.name.equals(entityName)) match {
        case -1 =>
          effectOnly(Effect(AjaxClient[Api].getMetaData(entityName).call().map(SetMetaDataForMenu(task, _))))
        case _ =>
          effectOnly(Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(task, pk, entityName)))
      }

    case SetMetaDataForMenu(task, entityMetaData) =>
      updated(entityMetaData :: value,Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(task, None, entityMetaData.entity.name)))

    case InitMetaData(entity) =>
      log.debug("InitMetaData ")
      effectOnly(Effect(AjaxClient[Api].getMetaData(entity).call().map(SetMetaData(_))))
    case SetMetaData(entityMetaData) =>
      log.debug("SetMetaData " + entityMetaData)
      updated(entityMetaData :: value)


    case FireActions(rulesCon: RulesContainer, actions: List[D2WAction]) =>
      if (actions.isEmpty) {
        noChange
      } else {
        log.debug("FireActions " + actions)

      // rulesContainer may have been updated, get it again from the model !
      val rulesContainer: RulesContainer = rulesCon match {
        case property: PropertyMetaInfo =>
          AppModel.propertyMetaDataWithEntityMetaDatas(value, property.entityName, property.task, property.name).get
        case task: Task =>
          // val entityMetaData = AppModel.entityMetaDataFromMegaContentForEntityNamed(value, entityMetaInfo.entity.name).get
          task // TBD refresh ?
      }


      // take first actions and call FireActions again with the rest
      val fireAction = actions.head
      val remainingActions = actions.tail
      fireAction match {
        case FireRule(rhs, key) =>
          log.debug("Fire Rule " + key + " context: " + rhs)
          // convert any rhs depending on previous results
          val newRhs = RuleUtils.convertD2WContextToFullFledged(rhs)
          effectOnly(Effect(AjaxClient[Api].fireRule(newRhs,key).call().map(rr =>
            {
              log.debug("rr " + rr)
              SetRuleResults(List(rr), rulesContainer, remainingActions)
            })))

        case CreateMemID(eo) =>
          log.debug("CreateMemID: " + eo)
          effectOnly(Effect.action(NewEOWithEntityName(eo.entity.name,rulesContainer,remainingActions)))


        // Hydration(
        //   DrySubstrate(None,None,Some(FetchSpecification(Customer,None))),
        //   WateringScope(Some(RuleFault(D2WContextFullFledged(Some(Project),Some(edit),Some(customer),None),keyWhenRelationship)))))
        //
        // // only on kind of Watering scope for the moment: property /ies from a rule. 2 cases:
        // // 1) displayPropertyKeys
        // // 2) keyWhenRelationship

        // Possible watering scopes (existing and future):
        //   1) Property from an already fired rule
        //   2) explicit propertyKeys

        // case class WateringScope(fireRule: Option[RuleFault] = None)
        // case class RuleFault(rhs: D2WContextFullFledged, key: String)


        case Hydration(drySubstrate,  wateringScope) =>
          // We handle only RuleFault
          // -> we expect it

          // get displayPropertyKeys from previous rule results

          // How to proceed:
          // Using the d2wContext and the key to fire. We look inside the existing rules to get the rule result
          val ruleResultOpt = RuleUtils.faultRule(wateringScope.fireRule.get, rulesContainer)
          log.debug("Hydration with scope defined by rule " + ruleResultOpt)

          ruleResultOpt match {
            case Some(RuleResult(rhs,key,value)) => {

              val ruleValue = rulesContainer.ruleResults
              val missingKeys: Set[String] = key match {
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
                  effectOnly(Effect(AjaxClient[Api].completeEO(eo,missingKeys).call().map(RefreshEO(_, rulesContainer, remainingActions))))
                case DrySubstrate(Some(eorefsdef), _ , _) =>
                  eorefsdef match {
                    case EORefsDefinition(Some(eoakp)) =>
                      val eovalueOpt = EOValueUtils.valueForKey(eoakp.eo, eoakp.keyPath)
                      eovalueOpt match {
                        case Some(eovalue) =>
                          val eoRefs = eovalue.eosV
                          effectOnly(Effect(AjaxClient[Api].hydrateEOs(eoRefs,missingKeys).call().map(FetchedObjectsForEntity(_, rulesContainer, remainingActions))))

                        case _ => effectOnly(Effect.action(FireActions(rulesContainer,remainingActions))) // we skip the action ....
                      }
                    case _ => effectOnly(Effect.action(FireActions(rulesContainer,remainingActions))) // we skip the action ....
                  }
                case DrySubstrate(_ , _ , Some(fs)) =>
                  effectOnly(Effect(AjaxClient[Api].search(fs.entityName, List.empty[QueryValue]).call().map(FetchedObjectsForEntity(_ , rulesContainer, remainingActions))))

                case _ =>  effectOnly(Effect.action(FireActions(rulesContainer,remainingActions))) // we skip the action ....
              }
            }
            case _ =>  effectOnly(Effect.action(FireActions(rulesContainer,remainingActions))) // we skip the action ....
          }
      }
    }



      //case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
      //                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
      //case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer

    // many rules
    case SetRuleResults(ruleResults, rulesContainer, actions: List[D2WAction]) =>
      log.debug("Rule Results " + ruleResults)
      log.debug("Rule Container " + rulesContainer)
      //val d2wContext = property.d2wContext
      //val entity = d2wContext.entity
      //val task = d2wContext.task
      rulesContainer match {
        case property: PropertyMetaInfo =>
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

        // case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer
        case task: Task =>
          val property = task.displayPropertyKeys.head
          val propertyKey = property.name
          val taskName = task.name

          // rule results are stored in EntityMetaData -> Task -> property
          val entityWriter = zoomToEntity(property.entityName,modelRW)
          entityWriter match {
            case Some(erw) => zoomToTask(taskName, erw) match {
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
}

class EOHandler[M](modelRW: ModelRW[M, Pot[EO]]) extends ActionHandler(modelRW) {

  override def handle = {
    case NewEOCreated(eo,property,actions) =>
      log.debug("eo created " + eo)
      updated(
        Ready(eo),
        Effect.action(FireActions(property, actions))
      )
    case RefreshEO(eo, property, actions) =>
      log.debug("Refreshed EO " + eo)
      updated(
        Ready(eo),
        Effect.action(FireActions(property,actions))
      )
    case EditEO(fromTask, eo) =>
      log.debug("EditEO: " + eo)
      updated(
        Ready(eo),
        Effect.action(InstallEditPage(fromTask,eo))
      )

  }
}


// hydrated destination EOs are simply stored in MegaContent eos
class EOCacheHandler[M](modelRW: ModelRW[M, EOCache]) extends ActionHandler(modelRW) {

  // eo chache is stored as:
  // Map of entityName -> Map of id -> eo
  // eos: Map[String, Map[Int,EO]],

  // Entity Name is retreived from the eos
  def updatedModelForEntityNamed(idExtractor: EO => Option[Int], cache: Map[String, Map[Int,EO]], eos: Seq[EO]): Map[String, Map[Int,EO]] = {
    if (eos.isEmpty) {
      cache
    } else {
      val result : Map[String, Map[Int,EO]] = newUpdatedCache(idExtractor, cache,eos)
      log.debug("storing result as :" + result)
      result
    }
  }

//EOValueUtils.pk(eo)
  def newUpdatedCache(idExtractor:EO => Option[Int], cache: Map[String, Map[Int,EO]],eos: Seq[EO]) = {
    val anyHead = eos.headOption
    anyHead match {
      case Some(head) =>
        val entity = head.entity
        val pkAttributeName = entity.pkAttributeName
        val entityName = entity.name
        val entityMap = if (cache.contains(entityName)) cache(entity.name) else Map.empty[Int,EO]

        // we create a Map with eo and id
        val refreshedEOs = eos.map(eo => {
          val pkOpt = idExtractor(eo)
          pkOpt match {
            case Some(pk) => Some((pk,eo))
            case _ => Some((-1,eo))
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

        cache + (entity.name -> newEntityMap)
      case _ => Map.empty[String, Map[Int,EO]]
    }
  }
  def updatedOutOfDBCacheWithEOs(eos: Seq[EO]): EOCache = {
    val insertedEOs = value.insertedEOs
    val outOfDBEOs = updatedModelForEntityNamed(eo => EOValueUtils.pk(eo),value.eos,eos)
    EOCache(outOfDBEOs,insertedEOs)
  }

  def updatedMemCacheWithEOs(eos: Seq[EO]): EOCache = {
    val newCache = updatedModelForEntityNamed(eo => eo.memID,value.insertedEOs,eos)
    EOCache(value.eos,newCache)
  }


  def updatedOutOfDBCache(eos: Map[String, Map[Int,EO]]): EOCache = {
    val insertedEOs = value.insertedEOs
    EOCache(eos,insertedEOs)
  }
  def updatedMemCache(eos: Map[String, Map[Int,EO]]): EOCache = {
    val dbEOs = value.eos
    EOCache(dbEOs,eos)
  }


  override def handle = {

    case FetchedObjectsForEntity(eoses, property, actions) =>
      log.debug("FetchedObjectsForEntity property " + property)

      updated(
        updatedOutOfDBCacheWithEOs(eoses),
        Effect.action(FireActions(property, actions))
      )

    case SearchResult(entity, eoses) =>
      log.debug("length " + eoses.length)
      updated(
        updatedOutOfDBCacheWithEOs(eoses),
        Effect(AfterEffectRouter.setListPageForEntity(entity.name))
      )
    case DeleteEOFromList(fromTask, eo) =>
      /*val eos = value.get
      val newEos = eos.filterNot(o => {o.id.equals(eo.id)})
      updated(Ready(newEos))*/
      effectOnly(Effect(AjaxClient[Api].deleteEO(eo).call().map( deletedEO => {
        val onError = deletedEO.validationError.isDefined
        if (onError) {
          log.debug("Deleted EO error " + deletedEO.validationError)
          UpdateEOsForEOOnError(deletedEO)

        } else {
          log.debug("Deleted EO action ")

          DeletedEO(deletedEO)
        }
      })))
    case DeletedEO(deletedEO) =>
      log.debug("Deleted EO " + deletedEO)
      val entityName = deletedEO.entity.name
      val eoPk = EOValueUtils.pk(deletedEO).get

      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap - eoPk
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))


    // set error on eo
    case UpdateEOsForEOOnError(eoOnError) =>
      val escapedHtml = Utils.escapeHtml(eoOnError.validationError.get)
      val eoWithDisplayableError = eoOnError.copy(validationError = Some(escapedHtml))
      val entityName = eoOnError.entity.name
      val eoPk = EOValueUtils.pk(eoWithDisplayableError).get
      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap + (eoPk -> eoWithDisplayableError)
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))

    case InspectEO(fromTask, eo) =>
      log.debug("InspectEO " + eo)
      effectOnly(
        Effect.action(InstallInspectPage(fromTask,eo))
      )

    case UpdateEOValueForProperty(eo, entityName, property, newEOValue) =>
      log.debug("Update EO Property: for entity " + entityName + " property: " + property + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      log.debug("EO: " + eo)
      val propertyName = property.name
      val newEO = eo.copy(values = (eo.values - propertyName) + (propertyName -> newEOValue))

      if (newEO.memID.isDefined) {
        updated(updatedMemCacheWithEOs(Seq(newEO)))
      } else {
        updated(updatedOutOfDBCacheWithEOs(Seq(newEO)))
      }

    case NewEOWithEOModel(eomodel, selectedEntityName, property, actions: List[D2WAction]) =>
      log.debug("CreateNewEO: " + selectedEntityName)
      // Create the EO and set it in the cache
      val (newValue, newEO) = EOValueUtils.createAndInsertNewObject(value.insertedEOs, eomodel, selectedEntityName)

      log.debug("newValue " + newValue)
      log.debug("newEO " + newEO)
      updated(updatedMemCache(newValue),
              Effect.action(NewEOCreated(newEO,property,actions)))



  }

}




class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case InitClient =>
      log.debug("Init Client")
      if (value.isEmpty) {
        log.debug("Api get Menus")
        effectOnly(Effect(AjaxClient[Api].getMenus().call().map(SetMenus)))
      } else
        noChange
    case InitMenuAndEO(eo, missingKeys) =>
      effectOnly(Effect(AjaxClient[Api].getMenus().call().map(menus => {
        SetMenusAndEO(menus, eo, missingKeys)
      })))

    case SetMenus(menus) =>
      log.debug("Set Menus " + menus)
      updated(Ready(menus),Effect.action(FetchEOModel))

    case SelectMenu(entityName) =>
      log.debug("selectedEntity " + entityName)
      updated(
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(entityName), task = Some("query")))),
        Effect.action(SetupQueryPageForEntity(entityName))
      )
    case SetPreviousPage(selectedEntity) =>
      log.debug("Set Previous Page for entity: " + selectedEntity)
      val previousTaskOpt = value.get.d2wContext.previousTask
      log.debug("PreviousTaskOpt: " + previousTaskOpt)

      previousTaskOpt match {
        case Some(previousTask) => {
          updated(
            // change context to inspect
            Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(task = Some(previousTask.task)))),
            Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(previousTask.task, previousTask.pk, selectedEntity.name))
          )
        }
        case _ => noChange
      }


    case Save(selectedEntityName, eo) =>
      log.debug("SAVE " + eo)
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
      log.debug("SAVE new eo " + eo)
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
      log.debug("Inspect page for entity " + eo)
      val pk = EOValueUtils.pk(eo).get
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(
          entityName = Some(eo.entity.name), // previousTask = Some(fromTask),
           task = Some("inspect")))),
        Effect(AfterEffectRouter.setInspectPageForEntity(value.get.d2wContext.entityName.get, pk))
      )
    case InstallEditPage(fromTask, eo) =>
      log.debug("Edit page for entity " + eo)
      val pk = EOValueUtils.pk(eo).get
      updated(
        // change context to inspect
        Ready(value.get.copy(d2wContext = value.get.d2wContext.copy(entityName = Some(eo.entity.name), // previousTask = Some(fromTask),
          task = Some("edit")))),
        Effect(AfterEffectRouter.setEditPageForEntity(value.get.d2wContext.entityName.get,pk))
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
      log.debug("Search: for entity " + selectedEntity + " with query values " + value)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)
      effectOnly(Effect(AjaxClient[Api].search(selectedEntity.name, value).call().map(SearchResult(selectedEntity, _))))
    //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")))
    //updated(value.copy(d2wContext = value.d2wContext.copy(entity = selectedEntity, task = "list")),Effect(AjaxClient[Api].search(EOKeyValueQualifier("name","Sw")).call().map(SearchResult)))
    //Effect(AjaxClient[Api].deleteTodo("1").call().map(noChange)))
    //

    case SetupQueryPageForEntity(selectedEntityName) =>
      updated(
        List(),
        Effect.action(SetPageForTaskAndEntity("query", selectedEntityName, None))
      )

    case UpdateQueryProperty(entityName, queryValue) =>
      log.debug("UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue)

      updated(queryValue :: value)
  }
}


