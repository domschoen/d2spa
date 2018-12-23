package d2spa.client.services

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate
import diode.ActionResult.ModelUpdateEffect
import d2spa.shared.{EO, EOValue, EntityMetaData, Menus, PropertyMetaInfo, _}
import boopickle.Default._
import d2spa.client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import japgolly.scalajs.react.extra.router.RouterCtl
import d2spa.client.SPAMain.TaskAppPage
import d2spa.client._
import d2spa.client.logger._
import d2spa.client.AppModel
import d2spa.shared.WebSocketMessages._


class SendingActionsHandler[M](modelRW: ModelRW[M, Set[Action]]) extends ActionHandler(modelRW) {
  override def handle = {
    case SendingAction(action) =>
      updated(value + action, Effect.action(action))
  }
}

class AppConfigurationHandler[M](modelRW: ModelRW[M, AppConfiguration]) extends ActionHandler(modelRW) {
  override def handle = {
    case FetchShowD2WDebugButton =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"DebugHandler | FetchShowD2WDebugButton")
      SPAMain.socket.send(GetDebugConfiguration)
      noChange

    case SetDebugConfiguration(debugConf) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"DebugHandler | SetShowD2WDebugButton " + debugConf.showD2WDebugButton)
      //val nextAction = if (value.fetchMenus) FetchEOModelAndMenus else FetchEOModel
      updated(value.copy(serverAppConf = DebugConf(debugConf.showD2WDebugButton)), Effect.action(FetchEOModelAndMenus))


    case SwithDebugMode =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"DebugHandler | SwithDebugMode")
      updated(value.copy(isDebugMode = !value.isDebugMode))

    // Action chain:
    // 1 SocketReady
    // 2 FetchShowD2WDebugButton
    // 3 SetDebugConfiguration
    // 4 FetchEOModelAndMenus or go directly to 8
    // 5 SetEOModelThenFetchMenu
    // 6 FetchMenu
    // 7 SetMenus
    // 8 FetchEOModel
    // 9 SetEOModel
    // 10 InitAppSpecificClient (to be implemented in the app specific part)
    case SocketReady =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"Socket Ready")
      updated(value.copy(socketReady = true), Effect.action(FetchShowD2WDebugButton))

  }
}

class BusyIndicatorHandler[M](modelRW: ModelRW[M, Boolean]) extends ActionHandler(modelRW) {
  override def handle = {
    case ShowBusyIndicator =>
      updated(true)
    case HideBusyIndicator =>
      updated(false)

    case SearchWithBusyIndicator(entityName) =>
      updated(true, Effect.action(SearchAction(entityName)))
  }
}



class RuleResultsHandler[M](modelRW: ModelRW[M, Map[String, Map[String, Map[String, PageConfigurationRuleResults]]]]) extends ActionHandler(modelRW) {

  def updatedRuleResults(ruleCache: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], ruleResults: List[RuleResult]) = {
    var updatedCache = ruleCache
    for (ruleResult <- ruleResults) {
      updatedCache = RuleUtils.registerRuleResult(updatedCache, ruleResult)
    }
    updatedCache
  }


  // case class RuleResult(rhs: D2WContextFullFledged, key: String, value: RuleValue)

  def ruleResultsWith(entityName : String, base: List[RuleResult], addOn: List[RuleResult]): List[RuleResult] = {
    D2SpaLogger.logfinest(entityName,"Mix base " + base)
    D2SpaLogger.logfinest(entityName,"+ addOn  " + addOn)

    val baseMap = base.map(x => ((x.rhs, x.key), x)).toMap
    val addOnMap = addOn.map(x => ((x.rhs, x.key), x)).toMap
    val mixMap = baseMap ++ addOnMap
    val result = mixMap.values.toList
    D2SpaLogger.logfinest(entityName,"result   " + result)
    result
  }

  def updatedRuleResultsWithEntityMetaData(d2wContext: D2WContext, entityMetaData: EntityMetaData) = {
    // convert data from entityMetaData to ruleResults
    val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
    val entityName = d2wContext.entityName.get

    D2SpaLogger.logfinest(entityName,"Register displayNameForEntity " + entityMetaData.displayName + " for task " + d2wContext.task)
    var updatedRuleResults = RuleUtils.registerRuleResult(value, RuleResult(fullFledged, RuleKeys.displayNameForEntity, RuleValue(stringV = Some(entityMetaData.displayName))))
    val displayPropertyKeys = entityMetaData.displayPropertyKeys map (p => p.name)
    updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, RuleResult(fullFledged, RuleKeys.displayPropertyKeys, RuleValue(stringsV = displayPropertyKeys)))

    for (prop <- entityMetaData.displayPropertyKeys) {
      val propertyD2WContext = fullFledged.copy(propertyKey = Some(prop.name))
      updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, RuleResult(propertyD2WContext, RuleKeys.propertyType, RuleValue(stringV = Some(prop.typeV))))
      for (ruleResult <- prop.ruleResults) {
        updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, ruleResult)
      }
    }
    updatedRuleResults
  }

  // handle actions
  override def handle = {

    /*case SetPageForTaskAndEntity(d2wContext) =>
      D2SpaLogger.logfinest(entityName,"SetPageForTaskAndEntity, d2wContext " + d2wContext)
      val entityName = d2wContext.entityName.get
      val metaDataPresent = RuleUtils.metaDataFetched(value,d2wContext)
      if (metaDataPresent) {
        D2SpaLogger.logfinest(entityName,"SetPageForTaskAndEntity, set page for entityName " + entityName)
        effectOnly(Effect.action(RegisterPreviousPage(d2wContext)))

      } else {
        D2SpaLogger.logfinest(entityName,"SetPageForTaskAndEntity, getMetaData for entityName " + entityName)
        val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
        effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaDataForMenu(d2wContext, _))))

      }

    case SetMetaDataForMenu(d2wContext, entityMetaData) => {
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults,Effect.action(RegisterPreviousPage(d2wContext)))
    }*/


    case FetchMetaData(d2wContext) =>
      log.finest("RuleResultsHandler | FireActions | FetchMetaData: ")
      //val taskFault: TaskFault = rulesCon match {case taskFault: TaskFault => taskFault}
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
      SPAMain.socket.send(WebSocketMessages.GetMetaData(fullFledged))
      noChange

    case SetMetaData(d2wContext, entityMetaData) =>
      val entityName = d2wContext.entityName.get
      //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData " + entityMetaData)
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData ")
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults)


    case FireActions(d2wContext: D2WContext, actions: List[D2WAction]) =>
      log.finest("RuleResultsHandler | FireActions | count: " + actions.size)
      for (action <- actions) {
        action match {
          case FireRule(rhs, key) =>
            log.finest("RuleResultsHandler | FireActions | FireRule")
            val newRhs = D2WContextUtils.convertFromD2WContextToFiringD2WContext(rhs)
            SPAMain.socket.send(WebSocketMessages.RuleToFire(newRhs, key))
          case FireRules(propertyKeys, rhs, key) =>
            for (propertyKey <- propertyKeys) {
              val firingRhs = D2WContextUtils.convertFromD2WContextToFiringD2WContext(rhs)

              val d2wContextProperty = firingRhs.copy(propertyKey = Some(propertyKey))
              SPAMain.socket.send(WebSocketMessages.RuleToFire(d2wContextProperty, key))
            }
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

          case Hydration(drySubstrate, wateringScope) =>
            log.finest("RuleResultsHandler | FireActions | Hydration: " + drySubstrate + " wateringScope: " + wateringScope)
            // We handle only RuleFault
            // -> we expect it

            // get displayPropertyKeys from previous rule results

            // How to proceed:
            // Using the d2wContext and the key to fire. We look inside the existing rules to get the rule result
            wateringScope match {
              case WateringScope(PotFiredRuleResult(Right(ruleResult))) =>
                log.finest("Hydration with scope defined by rule " + ruleResult)
                log.finest("Hydration drySubstrate " + drySubstrate)

                //ruleResult match {
                //  case Some(RuleResult(rhs, key, value)) => {

                //val ruleValue = rulesContainer.ruleResults
                val missingKeys: Set[String] = ruleResult.key match {
                  case RuleKeys.keyWhenRelationship =>
                    Set(ruleResult.value.stringV.get)
                  //Set(value)
                  case RuleKeys.displayPropertyKeys =>
                    ruleResult.value.stringsV.toSet
                  //Set(value)
                }
                log.finest("Hydration missingKeys " + missingKeys)

                drySubstrate match {
                  case DrySubstrate(_, Some(eoFault), _) =>
                    // completeEO ends up with a MegaContent eo update
                    log.finest("Hydration Call server with eo " + eoFault.pk + " missingKeys " + missingKeys)
                    // Will get response with FetchedObjectsMsgOut and then FetchedObjectsForEntity
                    SPAMain.socket.send(WebSocketMessages.CompleteEO(eoFault, missingKeys))

                  case DrySubstrate(Some(eoakp), _, _) =>
                    log.finest("Hydration DrySubstrate " + eoakp.eo.entityName + " for key " + eoakp.keyPath)
                    val eovalueOpt = EOValue.valueForKey(eoakp.eo, eoakp.keyPath)
                    log.finest("Hydration DrySubstrate valueForKey " + eovalueOpt)

                    eovalueOpt match {
                      case Some(eovalue) =>
                        eovalue match {
                          case ObjectsValue(pks) =>
                            log.finest("NVListComponent render pks " + pks)
                            // Will get response with FetchedObjectsMsgOut and then FetchedObjectsForEntity
                            SPAMain.socket.send(WebSocketMessages.HydrateEOs(ruleResult.rhs.entityName.get, pks, missingKeys))

                          case _ => // we skip the action ....
                        }
                      case _ => // we skip the action ....
                    }
                  case DrySubstrate(_, _, Some(fs)) =>
                    log.finest("Hydration with fs " + fs)
                    fs match {
                      case fa: EOFetchAll =>
                        // Will get response with FetchedObjectsMsgOut and then FetchedObjectsForEntity
                        SPAMain.socket.send(WebSocketMessages.HydrateAll(fa))
                      case fq: EOQualifiedFetch =>
                        // Will get response with FetchedObjectsMsgOut and then FetchedObjectsForEntity
                        SPAMain.socket.send(WebSocketMessages.Hydrate(fq))
                    }


                  case _ => // we skip the action ....
                }
              case WateringScope(PotFiredRuleResult(Left(ruleToFile))) =>
                log.finest("RuleResultsHandler | FireActions | Hydration:  wateringScope with rule To File: ")

                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(value, ruleToFile.rhs, ruleToFile.key)
                ruleResultOpt match {
                  case Some(ruleResult) =>
                    log.finest("RuleResultsHandler | FireActions | Hydration:  wateringScope with rule To File: existing rule result: " + ruleResult)

                    val updatedAction = Hydration(drySubstrate, WateringScope(PotFiredRuleResult(Right(ruleResult))))
                    val updatedActions = List(updatedAction)
                    effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))

                  case None =>

                    val newRhs = D2WContextUtils.convertFromD2WContextToFiringD2WContext(ruleToFile.rhs)
                    log.finest("RuleResultsHandler | FireActions | Hydration: wateringScope with rule To File: firing | rhs " + newRhs)
                    log.finest("RuleResultsHandler | FireActions | Hydration: wateringScope with rule To File: firing | key " + ruleToFile.key)
                    SPAMain.socket.send(WebSocketMessages.RuleToFire(newRhs, ruleToFile.key))

                }
            }
        }
      }
      noChange



    //case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
    //                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
    //case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer

    // many rules
    case SetRuleResults(ruleResults, d2wContext, actions: List[D2WAction]) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults")
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | ruleResults: " + ruleResults)
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | actions: " + actions)
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | d2wContext: " + d2wContext)
      //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetRuleResults " + ruleResults + " in d2w context " + d2wContext)
      //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetRuleResults | actions: " + actions)
      var updatedRuleResults = value
      for (ruleResult <- ruleResults) {
        updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, ruleResult)
      }
      updated(updatedRuleResults, Effect.action(FireActions(d2wContext, actions)))


    case SetJustRuleResults(ruleResults) =>
      log.finest("RuleResultsHandler | SetJustRuleResults | ruleResults " + ruleResults)
      val newRuleResults = updatedRuleResults(value,ruleResults)
      updated(newRuleResults)

  }

}


// hydrated destination EOs are simply stored in MegaContent eos
class EOCacheHandler[M](modelRW: ModelRW[M, EOCache]) extends ActionHandler(modelRW) {

  // eo chache is stored as:
  // Map of entityName -> Map of id -> eo
  // eos: Map[String, Map[Int,EO]],




  def updatedMemCacheWithEOs(entityName: String, eos: Seq[EO]): EOCache = {
    EOCacheUtils.updatedMemCacheWithEOsForEntityNamed(value, eos.toList, entityName)
  }


  /*def updatedOutOfDBCache(eos: Map[String, Map[EOPk, EO]]): EOCache = {
    val insertedEOs = value.insertedEOs
    EOCache(value.eomodel, eos, insertedEOs)
  }*/

  /*def updatedMemCache(eos: Map[String, Map[EOPk, EO]]): EOCache = {
    value.copy(insertedEOs = eos)
  }*/

  /*def addEOToDBCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    addEOToCache(eo, eos)
  }

  def addEOToMemCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    addEOToCache(eo, eos)
  }*/

  /*def addEOToCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    EOCacheUtils.updatedModelForEntityNamed(value.eomodel.get, eos, Seq(eo))
  }*/

  def removeEOFromDBCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    EOCacheUtils.removeEOFromCache(eo,  eos)
  }

  def removeEOFromMemCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    EOCacheUtils.removeEOFromCache(eo, eos)
  }



  override def handle = {


    case FetchEOModelAndMenus =>
      log.finest("FetchEOModel")
      SPAMain.socket.send(WebSocketMessages.FetchEOModel)
      noChange

    case SetEOModelThenFetchMenu(eomodel) =>
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(FetchMenu))


    case SetEOModel(eomodel) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"FetchEOModel set eomodel ")
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(InitAppSpecificClient))




    /*case NewEOWithEntityNameForEdit(selectedEntityName) =>
      D2SpaLogger.logfinest(entityName,"EOModelHandler | NewEOWithEntityNameForEdit " + selectedEntityName)
      effectOnly(
        Effect.action(NewEOWithEOModelForEdit(value.get, selectedEntityName)) // from edit ?
      )*/

    case EditEO(fromTask, eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | EditEO | register faulty eo  " + eo)

      // Adjust the db cache
      val newCache = updatedMemCacheWithEOs(entityName, Seq(eo))
      updated(
        newCache
      )


    // EO goes from inserted EO to db eos
    case SavedEO(fromTask, eo) =>
      val entityName = eo.entityName
      val eomodel = value.eomodel.get
      D2SpaLogger.logfinest(entityName," v " + eo)
      val isNewEO = EOValue.isNewEO(eo)
      val updatedEO = if (isNewEO) {
        val pk = EOValue.pk(eomodel, eo)
        D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | pk out of EO  " + pk)

        eo.copy(pk = pk.get)
      } else eo

      val newCache = EOCacheUtils.updatedCachesForSavedEO(value, updatedEO, isNewEO)
      val d2WContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.inspect), eo = Some(updatedEO))
      D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO update cache, call action Register with context " + d2WContext)
      updated(
        newCache,
        Effect.action(RegisterPreviousPageAndSetPage(d2WContext))
      )

    case Save(selectedEntityName, eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | SAVE " + eo)
      val purgedEO = EOValue.purgedEO(eo)

      // Update the DB and dispatch the result withing UpdatedEO action
      //
      SPAMain.socket.send(WebSocketMessages.UpdateEO(purgedEO))
      noChange

    // 1) NewEO (MenuHandler)
    // 2) Effect: Save DB
    // 3) SavedEO (EOCache)
    // 4) InstallInspectPage (MenuHandler)
    case SaveNewEO(entityName, eo) =>
      D2SpaLogger.logfinest(entityName,"CacheHandler | SaveNewEO " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      // Will get response with SavingEO
      SPAMain.socket.send(WebSocketMessages.NewEO(entityName, eo))
      noChange

    case SavingEO(eo) =>
      D2SpaLogger.logfinest(eo.entityName,"CacheHandler | SavingEO " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      val onError = eo.validationError.isDefined
      val action = if (onError) {
          // TODO implement it
          EditEO("edit", eo)

      } else {
          SavedEO("edit", eo)
      }
      effectOnly(Effect.action(action))


    // Update EO, stay on same page
    // Examples:
    //   - Save error -> update error in EO
    //   - New eo from server
    /*case UpdateEOInCache(eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | UpdateEOInCache " + eo.entityName)
      updated(
        EOCacheUtils.updatedCacheWithDBEOs(value.eomodel.get, value,Seq(eo))
      )

    case UpdateRefreshEOInCache(eos, property, actions) =>
      eos map(eo => {
        val entityName = eo.entityName
        D2SpaLogger.logfinest(entityName,"CacheHandler | Refreshed EOs " + entityName)
        D2SpaLogger.logfinest(entityName,"CacheHandler | Refreshed EO " + eo.values)
      })
      updated(
        EOCacheUtils.updatedCacheWithDBEOs(value.eomodel.get, value,eos),
        Effect.action(FireActions(property, actions))
      )


    case RefreshedEOs(eoses) =>
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get

      updated(
        EOCacheUtils.updatedCacheWithDBEOs(value.eomodel.get, value,eoses)
      )

    case FetchedObjectsForEntity(entityName, eos) =>
      log.finest("CacheHandler | FetchedObjectsForEntity eoses " + eos)
      val entity = EOModelUtils.entityNamed(value.eomodel.get,entityName).get

      updated(
        EOCacheUtils.updatedCacheWithDBEOs(entity, value, eos)
      )*/


    case SearchResult(fs, eoses) =>
      val entityName = EOFetchSpecification.entityName(fs)
      D2SpaLogger.logfinest(entityName,"CacheHandler | SearchResult length " + eoses.length)
      D2SpaLogger.logfinest(entityName,"CacheHandler | SearchResult before cache " + value)

      val action = eoses.length match {
        case x if x == 1 => {
          val eo = eoses.head
          InspectEO(TaskDefine.query,eo,true)
        }
        case _ => ShowResults(fs)
      }

      updated(
        EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eoses.toList, entityName),
        Effect.action(action)
      )


    case DeleteEOFromList(eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | DeleteEOFromList " + eo)

      SPAMain.socket.send(WebSocketMessages.DeleteEOMsgIn(eo))
      noChange


    case DeletingEO(deletedEO) =>
      log.finest("CacheHandler | DeletingEO " + deletedEO)

      /*val eos = value.get
      val newEos = eos.filterNot(o => {o.id.equals(eo.id)})
      updated(Ready(newEos))*/
      val onError = deletedEO.validationError.isDefined
      if (onError) {
        log.finest("Deleted EO error " + deletedEO.validationError)
        effectOnly(Effect.action(UpdateEOsForEOOnError(deletedEO)))

      } else {
        log.finest("Deleted EO action ")

        effectOnly(Effect.action(DeletedEO(deletedEO)))
      }


    case DeletedEO(deletedEO) =>
      val entityName = deletedEO.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | Deleted EO " + deletedEO)

      val newValue = EOCacheUtils.updatedDBCacheByDeletingEO(value,deletedEO)
      updated(newValue)



    // set error on eo
    case UpdateEOsForEOOnError(eoOnError) =>
      val entityName = eoOnError.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | UpdateEOsForEOOnError " + eoOnError)
      val eo = EOValue.escapeValidationError(eoOnError)
      val newValue = EOCacheUtils.updatedDBCacheWithEO(value,eo)
      updated(newValue)


    case UpdateEOValueForProperty(eo, d2wContext, newEOValue) =>
      println("UpdateEOValueForProperty")
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get

      D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: for entity " + entityName + " property: " + propertyName + " " + newEOValue)

      D2SpaLogger.logfinest(entityName, "EO: " + eo)
      val noValidationErrorEO = eo.copy(validationError = None)
      val updatedEO = EOValue.takeValueForKey(noValidationErrorEO, newEOValue, propertyName)
      D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: updatedEO " + updatedEO)

      if (EOValue.isNew(updatedEO.pk)) {
        D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: updatedMemCacheWithEOs " + updatedEO)
        updated(EOCacheUtils.updatedMemCacheWithEO(value, updatedEO))
      } else {
        D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: updatedOutOfDBCacheWithEOs " + updatedEO)
        updated(EOCacheUtils.updatedDBCacheWithEO(value, updatedEO))
      }

    case RegisteredExistingEO(d2wContext) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"CacheHandler | RegisteredExistingEO: " + d2wContext)
      val entityName = d2wContext.entityName.get
      // Create the EO and set it in the cache

      println("entityName " + entityName)
      val newCache = EOCacheUtils.updatedDBCacheWithEO(value, d2wContext.eo.get)
      println("newCache " + newCache)

      updated(newCache,Effect.action(RegisterPreviousPage(d2wContext)))

    case NewAndRegisteredEO(d2wContext) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"CacheHandler | NewEOWithEOModel: " + d2wContext)
      val entityName = d2wContext.entityName.get
      // Create the EO and set it in the cache

      println("value.insertedEOs " + value.insertedEOs)
      println("entityName " + entityName)
      val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(value, entityName)
      D2SpaLogger.logfinest(entityName, "newEO " + newEO)
      val updatedD2WContext = d2wContext.copy(eo = Some(newEO))

      updated(newCache,Effect.action(RegisterPreviousPage(updatedD2WContext)))



  }

}


class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {

    // server response will come with SetMenus
    case FetchMenu =>
      log.finest("Init Client")
      if (value.isEmpty) {
        log.finest("Api get Menus")
        SPAMain.socket.send(FetchMenus)
      }
      noChange


    case SetMenus(menus) =>
      log.finest("Set Menus " + menus)
      updated(Ready(menus), Effect.action(InitAppSpecificClient))


  }

}

