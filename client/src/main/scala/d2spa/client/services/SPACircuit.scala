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
    new EntityMetaDataHandler(zoomTo(_.content.entityMetaDatas)),
    //new EOHandler(zoomTo(_.content.editEOFault)),
    new EOCacheHandler(zoomTo(_.content.cache)),
    //new QueryValuesHandler(zoomTo(_.content.queryValues)),
    new EOModelHandler(zoomTo(_.content.eomodel)),
    new PreviousPageHandler(zoomTo(_.content.previousPage))
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


    // get the eomodel
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

class EntityMetaDataHandler[M](modelRW: ModelRW[M, List[EntityMetaData]]) extends ActionHandler(modelRW) {

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
    case SetPageForTaskAndEntity(d2WContext) =>
      log.debug("SetPageForTaskAndEntity, d2WContext " + d2WContext)
      val entityName = d2WContext.entityName.get
      value.indexWhere(n => n.entity.name.equals(entityName)) match {
        case -1 =>
          log.debug("SetPageForTaskAndEntity, getMetaData for entityName " + entityName)
          effectOnly(Effect(AjaxClient[Api].getMetaData(entityName).call().map(SetMetaDataForMenu(d2WContext, _))))
        case _ =>
          log.debug("SetPageForTaskAndEntity, set page for entityName " + entityName)
          effectOnly(Effect.action(RegisterPreviousPage(d2WContext)))
      }

    case SetMetaDataForMenu(d2WContext, entityMetaData) =>
      updated(entityMetaData :: value,Effect.action(RegisterPreviousPage(d2WContext)))


    case SetMetaDataWithActions(taskName, actions, entityMetaData) =>
      val task = EntityMetaDataUtils.taskWithTaskName(entityMetaData,taskName)
      updated(entityMetaData :: value,Effect.action(FireActions(task, actions)))


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
        case taskFault: TaskFault =>
          taskFault
      }


      // take first actions and call FireActions again with the rest
      val fireAction = actions.head
      val remainingActions = actions.tail
      fireAction match {
        case FireRule(rhs, key) =>
          log.debug("Fire Rule " + key + " context: " + rhs)
          // convert any rhs depending on previous results
          val newRhs = D2WContextUtils.convertD2WContextToFullFledged(rhs)
          effectOnly(Effect(AjaxClient[Api].fireRule(newRhs,key).call().map(rr =>
            {
              log.debug("rr " + rr)
              SetRuleResults(List(rr), rulesContainer, remainingActions)
            })))

        case CreateMemID(entityName) =>
          log.debug("CreateMemID: " + entityName)
          effectOnly(Effect.action(NewEOWithEntityName(entityName,rulesContainer,remainingActions)))

        case FetchMetaData(entityName) =>
          val taskFault: TaskFault = rulesCon match {case taskFault: TaskFault => taskFault}
          effectOnly(Effect(AjaxClient[Api].getMetaData(entityName).call().map(SetMetaDataWithActions(taskFault.taskName, remainingActions, _))))


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
                case DrySubstrate(_ , Some(eoFault), _) =>
                  // completeEO ends up with a MegaContent eo update
                  effectOnly(Effect(AjaxClient[Api].completeEO(eoFault,missingKeys).call().map(UpdateRefreshEOInCache(_, rulesContainer, remainingActions))))
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

class EOHandler[M](modelRW: ModelRW[M, EditEOFault]) extends ActionHandler(modelRW) {

  override def handle = {



    case NewEOCreated(eo,property,actions) =>
      log.debug("eo created " + eo)
      updated(
        EditEOFault(Ready(eo),value.newCounter),
        Effect.action(FireActions(property, actions))
      )
    case EditEO(fromTask, eo) =>
      log.debug("EditEO: " + eo)
      updated(
        EditEOFault(Ready(eo),value.newCounter),
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
  def updatedModelForEntityNamed(idExtractor: EO => Option[Int], cache: Map[String, Map[Int, EO]], eos: Seq[EO]): Map[String, Map[Int, EO]] = {
    if (eos.isEmpty) {
      cache
    } else {
      val result: Map[String, Map[Int, EO]] = newUpdatedCache(idExtractor, cache, eos)
      log.debug("storing result as :" + result)
      result
    }
  }

  //EOValueUtils.pk(eo)
  def newUpdatedCache(idExtractor: EO => Option[Int], cache: Map[String, Map[Int, EO]], eos: Seq[EO]) = {
    val anyHead = eos.headOption
    anyHead match {
      case Some(head) =>
        val entity = head.entity
        val pkAttributeName = entity.pkAttributeName
        val entityName = entity.name
        val entityMap = if (cache.contains(entityName)) cache(entity.name) else Map.empty[Int, EO]

        // we create a Map with eo and id
        val refreshedEOs = eos.map(eo => {
          val pkOpt = idExtractor(eo)
          pkOpt match {
            case Some(pk) => Some((pk, eo))
            case _ => Some((-1, eo))
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
            (id, EOValueUtils.completeEoWithEo(existingEO, refreshedEO))
          }
        }).toMap
        val newEntityMap = entityMap ++ newAndUpdateMap

        cache + (entity.name -> newEntityMap)
      case _ => Map.empty[String, Map[Int, EO]]
    }
  }

  def updatedOutOfDBCacheWithEOs(eos: Seq[EO]): EOCache = {
    val insertedEOs = value.insertedEOs
    val outOfDBEOs = updatedModelForEntityNamed(eo => EOValueUtils.pk(eo), value.eos, eos)
    EOCache(outOfDBEOs, insertedEOs)
  }


  def updatedMemCacheWithEOs(eos: Seq[EO]): EOCache = {
    val newCache = updatedModelForEntityNamed(eo => eo.memID, value.insertedEOs, eos)
    EOCache(value.eos, newCache)
  }


  def updatedOutOfDBCache(eos: Map[String, Map[Int, EO]]): EOCache = {
    val insertedEOs = value.insertedEOs
    EOCache(eos, insertedEOs)
  }

  def updatedMemCache(eos: Map[String, Map[Int, EO]]): EOCache = {
    val dbEOs = value.eos
    EOCache(dbEOs, eos)
  }

  def addEOToDBCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    addEOToCache(eo, e => EOValueUtils.pk(e), eos)
  }
  def addEOToMemCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    addEOToCache(eo, e => e.memID, eos)
  }

  def addEOToCache(eo: EO, idExtractor: EO => Option[Int], eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    updatedModelForEntityNamed(idExtractor, eos, Seq(eo))
  }

  def removeEOFromDBCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    removeEOFromCache(eo, e => EOValueUtils.pk(e), eos)
  }

  def removeEOFromMemCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    removeEOFromCache(eo, e => e.memID, eos)
  }

  def removeEOFromCache(eo: EO, idExtractor: EO => Option[Int], eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    val entityName = eo.entity.name
    val eoID = idExtractor(eo)
    eoID match {
      case Some(id) =>
        val entityCache = eos(entityName)
        val updatedEntityCache = entityCache - id
        val updatedCache = eos + (entityName -> updatedEntityCache)
        updatedCache
      case _ => eos
    }
  }




  override def handle = {

    // EO goes from inserted EO to db eos
    case SavedEO(fromTask, eo) =>
      log.debug("SavedEO " + eo)
      val insertedEOs = removeEOFromMemCache(eo,value.insertedEOs)
      val eos = addEOToDBCache(eo,value.eos)
      val pk = EOValueUtils.pk(eo)
      val d2WContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.inspect), eo = Some(D2WContextEO(pk,None)))
      updated(
        EOCache(eos,insertedEOs),
        Effect.action(RegisterPreviousPage(d2WContext))
      )

    case Save(selectedEntityName, eo) =>
      log.debug("SAVE " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      effectOnly(Effect(AjaxClient[Api].updateEO(eo).call().map(savingEO => {
        val onError = savingEO.validationError.isDefined
        if (onError) {
          UpdateEOInCache(savingEO)

        } else {
          SavedEO("edit", savingEO)
        }
      }
      ))
      )

    case CreateEO(entityName) =>
      // Needs to look in the mem cache for the next memID of entityName
      // Add to inserted eo

    case UpdateEOInCache(eo) =>
      log.debug("UpdateEOInCache " + eo)
      updated(
        updatedOutOfDBCacheWithEOs(Seq(eo))
      )

    case UpdateRefreshEOInCache(eo, property, actions) =>
      log.debug("Refreshed EO " + eo)
      updated(
        updatedOutOfDBCacheWithEOs(Seq(eo)),
        Effect.action(FireActions(property,actions))
      )

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

class PreviousPageHandler[M](modelRW: ModelRW[M, Option[D2WContext]]) extends ActionHandler(modelRW) {
  override def handle = {
    case RegisterPreviousPage(d2WContext) =>
      log.debug("Set Previous Page for d2wContext: " + d2WContext)
      val lastPage = value match {
        case Some(currentPage) => d2WContext.copy(previousTask = value)
        case _ => d2WContext
      }
      updated(
            // change context to inspect
            Some(lastPage),
            Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(d2WContext))
          )

    case SetPreviousPage =>
      log.debug("SetPreviousPage to: " + value)
      value match {
        case Some(previousTask) => {
          updated(
            // change context to inspect
            if (value.isDefined) value.get.previousTask else None,
            Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(previousTask))
          )
        }
        case _ => noChange
      }


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




    // 1) NewEO (MenuHandler)
    // 2) Effect: Save DB
    // 3) SavedEO (EOCache)
    // 4) InstallInspectPage (MenuHandler)
    case SaveNewEO(selectedEntity, eo) =>
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
            SavedEO("edit", newEO)
          }
        }
        ))
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

  // 1) Search (QueryValuesHandler)
  // 2) Effect: fetch
  // 3) SearchResult (EOCacheHandler)
  // 4) Effect: set list page
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
      val d2wContext = D2WContext(Some(selectedEntityName),Some(TaskDefine.query),None,0,None,None,None)
      updated(
        List(),
        Effect.action(SetPageForTaskAndEntity(d2wContext))
      )

    case UpdateQueryProperty(entityName, queryValue) =>
      log.debug("UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue)

      updated(queryValue :: value)
  }
}


