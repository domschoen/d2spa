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

import d2spa.shared.{Menus, EntityMetaData, PropertyMetaInfo, EO, EOValue, DebugConf}



class AppConfigurationHandler[M](modelRW: ModelRW[M, AppConfiguration]) extends ActionHandler(modelRW) {
  override def handle = {
    case FetchShowD2WDebugButton =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"DebugHandler | FetchShowD2WDebugButton")
      effectOnly(Effect(AjaxClient[Api].getDebugConfiguration().call().map(SetDebugConfiguration(_))))

    case SetDebugConfiguration(debugConf) =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"DebugHandler | SetShowD2WDebugButton " + debugConf.showD2WDebugButton)
      val nextAction = if (value.fetchMenus) FetchEOModelAndMenus else FetchEOModel
      updated(value.copy(serverAppConf = DebugConf(debugConf.showD2WDebugButton)), Effect.action(nextAction))

    case SwithDebugMode =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"DebugHandler | SwithDebugMode")
      updated(value.copy(isDebugMode = !value.isDebugMode))
  }
}

class BusyIndicatorHandler[M](modelRW: ModelRW[M, Boolean]) extends ActionHandler(modelRW) {
  override def handle = {
    case ShowBusyIndicator =>
      updated(true)
    case HideBusyIndicator =>
      updated(false)

    case SearchWithBusyIndicator(entityName) =>
      updated(true, Effect.action(Search(entityName)))
  }
}



class RuleResultsHandler[M](modelRW: ModelRW[M, Map[String, Map[String, Map[String, PageConfigurationRuleResults]]]]) extends ActionHandler(modelRW) {


  // case class RuleResult(rhs: D2WContextFullFledged, key: String, value: RuleValue)

  def ruleResultsWith(entityName : String, base: List[RuleResult], addOn: List[RuleResult]): List[RuleResult] = {
    D2SpaLogger.logDebug(entityName,"Mix base " + base)
    D2SpaLogger.logDebug(entityName,"+ addOn  " + addOn)

    val baseMap = base.map(x => ((x.rhs, x.key), x)).toMap
    val addOnMap = addOn.map(x => ((x.rhs, x.key), x)).toMap
    val mixMap = baseMap ++ addOnMap
    val result = mixMap.values.toList
    D2SpaLogger.logDebug(entityName,"result   " + result)
    result
  }

  def updatedRuleResultsWithEntityMetaData(d2wContext: D2WContext, entityMetaData: EntityMetaData) = {
    // convert data from entityMetaData to ruleResults
    val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
    val entityName = d2wContext.entityName.get

    D2SpaLogger.logDebug(entityName,"Register displayNameForEntity " + entityMetaData.displayName + " for task " + d2wContext.task)
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
      D2SpaLogger.logDebug(entityName,"SetPageForTaskAndEntity, d2wContext " + d2wContext)
      val entityName = d2wContext.entityName.get
      val metaDataPresent = RuleUtils.metaDataFetched(value,d2wContext)
      if (metaDataPresent) {
        D2SpaLogger.logDebug(entityName,"SetPageForTaskAndEntity, set page for entityName " + entityName)
        effectOnly(Effect.action(RegisterPreviousPage(d2wContext)))

      } else {
        D2SpaLogger.logDebug(entityName,"SetPageForTaskAndEntity, getMetaData for entityName " + entityName)
        val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
        effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaDataForMenu(d2wContext, _))))

      }

    case SetMetaDataForMenu(d2wContext, entityMetaData) => {
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults,Effect.action(RegisterPreviousPage(d2wContext)))
    }*/


    case SetMetaDataWithActions(d2wContext, actions, entityMetaData) =>
      //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | SetMetaDataWithActions " + entityMetaData + " actions: " + actions)

      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults, Effect.action(FireActions(d2wContext, actions)))

    case SetMetaData(d2wContext, entityMetaData) =>
      val entityName = d2wContext.entityName.get
      //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | SetMetaData " + entityMetaData)
      D2SpaLogger.logDebug(entityName,"RuleResultsHandler | SetMetaData ")
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults)


    case FireActions(d2wContext: D2WContext, actions: List[D2WAction]) =>
      if (actions.isEmpty) {
        noChange
      } else {
        D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | FireActions " + actions.size)
        //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions " + actions.size + " actions: " + actions)


        // take first actions and call FireActions again with the rest
        val fireAction = actions.head
        val remainingActions = actions.tail
        fireAction match {
          case FireRule(rhs, key) =>


            //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | should Fire Rule ? " + key + " context: " + rhs)
            D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | FireActions | FireRule | should Fire Rule ? " + key)
            // convert any rhs depending on previous results

            //val newRhs = D2WContextUtils.convertD2WContextToFullFledgedByResolvingRuleFaults(value, rhs)
            rhs.pageConfiguration match {
              case PotFiredKey(Right(value)) =>
                val newRhs = D2WContextUtils.convertD2WContextToFullFledged(rhs)

                //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Fire Rule | convertD2WContextToFullFledged " + newRhs)
                D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | FireActions | Fire Rule | key " + key)


                effectOnly(Effect(AjaxClient[Api].fireRule(newRhs, key).call().map(rr => {
                  //D2SpaLogger.logDebug(entityName,"rr " + rr)
                  SetRuleResults(List(rr), d2wContext, remainingActions)
                })))


              case PotFiredKey(Left(ruleToFile)) =>

                //
                D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | FireActions | FireRule | " + key + " page Configuration is Left " + ruleToFile.key)
                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(value, ruleToFile.rhs, ruleToFile.key)
                ruleResultOpt match {
                  case Some(ruleResult) =>
                    val updatedAction = FireRules(KeysSubstrate(PotFiredRuleResult(Right(ruleResult))), rhs, key)
                    val updatedActions = updatedAction :: remainingActions
                    effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))

                  case None =>
                    val fullFledgedRhs = D2WContextUtils.convertD2WContextToFullFledged(ruleToFile.rhs)


                    effectOnly(Effect(AjaxClient[Api].fireRule(fullFledgedRhs, ruleToFile.key).call().map(rr => {
                      //D2SpaLogger.logDebug(entityName,"rr " + rr)
                      SetRuleResults(List(rr), d2wContext, actions)
                    })))
                }
            }

          case CreateMemID(entityName) =>
            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | CreateMemID: " + entityName)
            effectOnly(Effect.action(NewEOWithEntityName(d2wContext, remainingActions)))

          case FetchMetaData(d2wContext) =>
            val entityName = d2wContext.entityName.get
            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | FetchMetaData: ")
            //val taskFault: TaskFault = rulesCon match {case taskFault: TaskFault => taskFault}
            val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
            effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaDataWithActions(d2wContext, remainingActions, _))))

          case FireRules(keysSubstrate, rhs, key) =>
            // TODO avoid double fetch

            val entityName = rhs.entityName.get

            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | FireRules")
            //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | FireRules: " + keysSubstrate)
            // For the moment, a KeySubstrate can have only a RuleFault
            // Let's resolve the fault (= firing the fault)

            keysSubstrate match {
              case KeysSubstrate(PotFiredRuleResult(Right(ruleResult))) =>
                //val ruleValue = rulesContainer.ruleResults
                val keys: Set[String] = ruleResult.key match {
                  case RuleKeys.keyWhenRelationship =>
                    Set(ruleResult.value.stringV.get)
                  //Set(value)
                  case RuleKeys.displayPropertyKeys =>
                    ruleResult.value.stringsV.toSet
                  //Set(value)
                }
                val dc = D2WContextUtils.convertFullFledgedToD2WContext(ruleResult.rhs)
                val rulesActions = keys.toList.map(k => {
                  val d2wContextProperty = dc.copy(propertyKey = Some(k))
                  FireRule(d2wContextProperty, key)
                })
                val updatedActions = remainingActions ::: rulesActions
                effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))

              case KeysSubstrate(PotFiredRuleResult(Left(ruleToFile))) =>
                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(value, ruleToFile.rhs, ruleToFile.key)
                ruleResultOpt match {
                  case Some(ruleResult) =>
                    val updatedAction = FireRules(KeysSubstrate(PotFiredRuleResult(Right(ruleResult))), rhs, key)
                    val updatedActions = updatedAction :: remainingActions
                    effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))

                  case None =>
                    val fullFledgedRhs = D2WContextUtils.convertD2WContextToFullFledged(ruleToFile.rhs)

                    effectOnly(Effect(AjaxClient[Api].fireRule(fullFledgedRhs, ruleToFile.key).call().map(rr => {
                      //D2SpaLogger.logDebug(entityName,"rr " + rr)
                      SetRuleResults(List(rr), d2wContext, actions)
                    })))

                }

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
            val entityName = Hydration.entityName(drySubstrate)
            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration: " +" wateringScope: " + wateringScope)
            // We handle only RuleFault
            // -> we expect it

            // get displayPropertyKeys from previous rule results

            // How to proceed:
            // Using the d2wContext and the key to fire. We look inside the existing rules to get the rule result
            wateringScope match {
              case WateringScope(PotFiredRuleResult(Right(ruleResult))) =>
                D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration with scope defined by rule " + ruleResult)
                D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration drySubstrate " + drySubstrate)

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
                D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration missingKeys " + missingKeys)

                drySubstrate match {
                  case DrySubstrate(_, Some(eoFault), _) =>
                    // completeEO ends up with a MegaContent eo update
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration Call server with eo " + eoFault.pk + " missingKeys " + missingKeys)
                    effectOnly(Effect(AjaxClient[Api].completeEO(eoFault, missingKeys).call().map(UpdateRefreshEOInCache(_, d2wContext, remainingActions))))

                  case DrySubstrate(Some(eoakp), _, _) =>
                    val eo = eoakp.eo
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration DrySubstrate " + eo.entityName + " for key " + eoakp.keyPath)
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration DrySubstrate values: " + eo.values)
                    val eovalueOpt = EOValue.valueForKey(eo, eoakp.keyPath)
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration DrySubstrate valueForKey " + eovalueOpt)

                    eovalueOpt match {
                      case Some(eovalue) =>
                        eovalue match {
                          case ObjectsValue(pks) =>
                            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration DrySubstrate render pks " + pks)
                            effectOnly(Effect(AjaxClient[Api].hydrateEOs(ruleResult.rhs.entityName.get, pks, missingKeys).call().map(FetchedObjectsForEntity(_, d2wContext, remainingActions))))
                          case ObjectValue(EO(de,_,pk,_)) =>
                            D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration DrySubstrate render pk " + pk)
                            effectOnly(Effect(AjaxClient[Api].completeEO(EOFault(de,pk), missingKeys).call().map(UpdateRefreshEOInCache(_, d2wContext, remainingActions))))

                          case _ => effectOnly(Effect.action(FireActions(d2wContext, remainingActions))) // we skip the action ....
                        }
                      case _ => effectOnly(Effect.action(FireActions(d2wContext, remainingActions))) // we skip the action ....
                    }
                  case DrySubstrate(_, _, Some(fs)) =>
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration with fs " + fs)
                    fs match {
                      case fa: EOFetchAll => effectOnly(Effect(AjaxClient[Api].searchAll(fa).call().map(FetchedObjectsForEntity(_, d2wContext, remainingActions))))
                      case fq: EOQualifiedFetch => effectOnly(Effect(AjaxClient[Api].search(fq).call().map(FetchedObjectsForEntity(_, d2wContext, remainingActions))))
                    }


                  case _ => effectOnly(Effect.action(FireActions(d2wContext, remainingActions))) // we skip the action ....
                }
              case WateringScope(PotFiredRuleResult(Left(ruleToFile))) =>
                D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | Hydration Left " + ruleToFile.key)
                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(value, ruleToFile.rhs,ruleToFile.key)
                ruleResultOpt match {
                  case Some(ruleResult) =>
                    D2SpaLogger.logDebug(entityName,"RuleResultsHandler | FireActions | rule to file has result " + ruleResult.value)

                    val updatedAction = Hydration(drySubstrate, WateringScope(PotFiredRuleResult(Right(ruleResult))))
                    val updatedActions = updatedAction :: remainingActions
                    effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))
                  case None =>
                    val fullFledgedRhs = D2WContextUtils.convertD2WContextToFullFledged(ruleToFile.rhs)

                    effectOnly(
                      Effect(
                        AjaxClient[Api].fireRule(fullFledgedRhs, ruleToFile.key).call().map(
                          rr => {
                            //D2SpaLogger.logDebug(entityName,"rr " + rr)
                            SetRuleResults(List(rr), d2wContext, actions)
                          }
                        )
                      )
                    )
                }
            }


        }

      }



    //case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
    //                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
    //case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer

    // many rules
    case SetRuleResults(ruleResults, d2wContext, actions: List[D2WAction]) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults")
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | ruleResults: " + ruleResults)
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | actions: " + actions)
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | d2wContext: " + d2wContext)
      //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | SetRuleResults " + ruleResults + " in d2w context " + d2wContext)
      //D2SpaLogger.logDebug(entityName,"RuleResultsHandler | SetRuleResults | actions: " + actions)
      var updatedRuleResults = value
      for (ruleResult <- ruleResults) {
        updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, ruleResult)
      }
      updated(updatedRuleResults, Effect.action(FireActions(d2wContext, actions)))

  }

}


// hydrated destination EOs are simply stored in MegaContent eos
class EOCacheHandler[M](modelRW: ModelRW[M, EOCache]) extends ActionHandler(modelRW) {

  // eo chache is stored as:
  // Map of entityName -> Map of id -> eo
  // eos: Map[String, Map[Int,EO]],




  def updatedMemCacheWithEOs(eos: Seq[EO]): EOCache = {
    val newCache = EOCacheUtils.updatedModelForEntityNamed(value.eomodel.get,value.insertedEOs, eos)
    EOCache(value.eomodel, value.eos, newCache)
  }


  def updatedOutOfDBCache(eos: Map[String, Map[List[Int], EO]]): EOCache = {
    val insertedEOs = value.insertedEOs
    EOCache(value.eomodel, eos, insertedEOs)
  }

  def updatedMemCache(eos: Map[String, Map[List[Int], EO]]): EOCache = {
    val dbEOs = value.eos
    EOCache(value.eomodel, dbEOs, eos)
  }

  def addEOToDBCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    addEOToCache(eo, eos)
  }

  def addEOToMemCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    addEOToCache(eo, eos)
  }

  def addEOToCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    EOCacheUtils.updatedModelForEntityNamed(value.eomodel.get, eos, Seq(eo))
  }

  def removeEOFromDBCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    removeEOFromCache(eo,  eos)
  }

  def removeEOFromMemCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    removeEOFromCache(eo, eos)
  }

  def removeEOFromCache(eo: EO, eos: Map[String, Map[List[Int], EO]]): Map[String, Map[List[Int], EO]] = {
    val entityName = eo.entityName
    val id = eo.pk
    val entityCache = eos(entityName)
    val updatedEntityCache = entityCache - id
    val updatedCache = eos + (entityName -> updatedEntityCache)
    updatedCache
  }


  override def handle = {
    // Action chain:
    // 1 InitClient
    // 2 FetchShowD2WDebugButton
    // 3 SetDebugConfiguration
    // 4 FetchEOModelAndMenus or go directly to 8
    // 5 SetEOModelThenFetchMenu
    // 6 FetchMenu
    // 7 SetMenus
    // 8 FetchEOModel
    // 9 SetEOModel
    // 10 InitAppSpecificClient (to be implemented in the app specific part)
    case InitClient =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"Init Client")
      effectOnly(Effect.action(FetchShowD2WDebugButton))


    case FetchEOModel =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"FetchEOModel")

      effectOnly(Effect(AjaxClient[Api].fetchEOModel().call().map(SetEOModel(_))))

    case FetchEOModelAndMenus =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"FetchEOModel")
      effectOnly(Effect(AjaxClient[Api].fetchEOModel().call().map(SetEOModelThenFetchMenu(_))))

    case SetEOModelThenFetchMenu(eomodel) =>
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(FetchMenu))


    case SetEOModel(eomodel) =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"FetchEOModel set eomodel ")
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(InitAppSpecificClient))


    // get the eomodel
    case NewEOWithEntityName(d2wContext, actions) =>
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logDebug(entityName,"NewEOWithEntityName: " + d2wContext)
      // Create the EO and set it in the cache
      effectOnly(
        Effect.action(NewEOWithEOModel(value.eomodel.get, d2wContext, actions)) // from edit ?
      )

    /*case NewEOWithEntityNameForEdit(selectedEntityName) =>
      D2SpaLogger.logDebug(entityName,"EOModelHandler | NewEOWithEntityNameForEdit " + selectedEntityName)
      effectOnly(
        Effect.action(NewEOWithEOModelForEdit(value.get, selectedEntityName)) // from edit ?
      )*/



    // EO goes from inserted EO to db eos
    case SavedEO(fromTask, eo) =>
      val entityName = eo.entityName
      val eomodel = value.eomodel.get
      D2SpaLogger.logDebug(entityName,"CacheHandler | SavedEO " + eo)
      val isNewEO = EOValue.isNew(eo)
      // Adjust the insertedEOs cache
      val insertedEOs = if (isNewEO) removeEOFromMemCache(eo, value.insertedEOs) else value.insertedEOs
      D2SpaLogger.logDebug(entityName,"CacheHandler | SavedEO | removed if new  " + isNewEO)
      val updatedEO = if (isNewEO) {
        val pk = EOValue.pk(eomodel, eo)
        eo.copy(pk = pk.get)
      } else eo
      D2SpaLogger.logDebug(entityName,"CacheHandler | SavedEO | register eo  " + updatedEO)

      // Adjust the db cache
      val eos = addEOToDBCache(updatedEO, value.eos)
      val d2WContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.inspect), eo = Some(updatedEO))
      D2SpaLogger.logDebug(entityName,"CacheHandler | SavedEO update cache, call action Register with context " + d2WContext)
      updated(
        EOCache(value.eomodel, eos, insertedEOs),
        Effect.action(RegisterPreviousPage(d2WContext))
      )

    case Save(selectedEntityName, eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logDebug(entityName,"CacheHandler | SAVE " + eo)
      val purgedEO = EOValue.purgedEO(eo)

      // Update the DB and dispatch the result withing UpdatedEO action
      effectOnly(Effect(AjaxClient[Api].updateEO(purgedEO).call().map(savingEO => {
        val onError = savingEO.validationError.isDefined
        if (onError) {
          UpdateEOInCache(savingEO)
        } else {
          SavedEO("edit", savingEO)
        }
      })))

    // 1) NewEO (MenuHandler)
    // 2) Effect: Save DB
    // 3) SavedEO (EOCache)
    // 4) InstallInspectPage (MenuHandler)
    case SaveNewEO(entityName, eo) =>
      D2SpaLogger.logDebug(entityName,"CacheHandler | SaveNewEO " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      effectOnly(Effect(AjaxClient[Api].newEO(entityName, eo).call().map(newEO => {
        val onError = newEO.validationError.isDefined
        if (onError) {

          // TODO implement it
          EditEO("edit", newEO)

        } else {
          SavedEO("edit", newEO)
        }
      }
      ))
      )


    // Update EO, stay on same page
    // Examples:
    //   - Save error -> update error in EO
    //   - New eo from server
    case UpdateEOInCache(eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logDebug(entityName,"CacheHandler | UpdateEOInCache " + eo.entityName)
      updated(
        EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value,Seq(eo))
      )

    case UpdateRefreshEOInCache(eos, property, actions) =>
      eos map(eo => {
        val entityName = eo.entityName
        D2SpaLogger.logDebug(entityName,"CacheHandler | Refreshed EOs " + entityName)
        D2SpaLogger.logDebug(entityName,"CacheHandler | Refreshed EO " + eo.values)
      })
      updated(
        EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value,eos),
        Effect.action(FireActions(property, actions))
      )


    case RefreshedEOs(eoses) =>
      updated(
        EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value,eoses)
      )

    case FetchedObjectsForEntity(eoses, d2wContext, actions) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"CacheHandler | FetchedObjectsForEntity d2wContext " + d2wContext)
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"CacheHandler | FetchedObjectsForEntity eoses " + eoses)

      updated(
        EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value,eoses),
        Effect.action(FireActions(d2wContext, actions))
      )

    case SearchResult(entityName, eoses) =>
      D2SpaLogger.logDebug(entityName,"CacheHandler | SearchResult length " + eoses.length)

      val action = eoses.length match {
        case x if x == 1 => {
          val eo = eoses.head
          InspectEO(TaskDefine.query,eo,true)
        }
        case _ => ShowResults
      }

      updated(
        EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value, eoses),
        Effect.action(action)
      )

    case DeleteEOFromList(fromTask, eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logDebug(entityName,"CacheHandler | DeleteEOFromList " + eo)

      /*val eos = value.get
      val newEos = eos.filterNot(o => {o.id.equals(eo.id)})
      updated(Ready(newEos))*/
      effectOnly(Effect(AjaxClient[Api].deleteEO(eo).call().map(deletedEO => {
        val onError = deletedEO.validationError.isDefined
        if (onError) {
          D2SpaLogger.logDebug(entityName,"Deleted EO error " + deletedEO.validationError)
          UpdateEOsForEOOnError(deletedEO)

        } else {
          D2SpaLogger.logDebug(entityName,"Deleted EO action ")

          DeletedEO(deletedEO)
        }
      })))
    case DeletedEO(deletedEO) =>
      val entityName = deletedEO.entityName
      D2SpaLogger.logDebug(entityName, "CacheHandler | Deleted EO " + deletedEO)
      val eoPk = EOValue.pk(value.eomodel.get, deletedEO).get

      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap - eoPk
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))


    // set error on eo
    case UpdateEOsForEOOnError(eoOnError) =>
      val entityName = eoOnError.entityName
      D2SpaLogger.logDebug(entityName, "CacheHandler | UpdateEOsForEOOnError " + eoOnError)
      val escapedHtml = Utils.escapeHtml(eoOnError.validationError.get)
      val eoWithDisplayableError = eoOnError.copy(validationError = Some(escapedHtml))
      val eoPk = EOValue.pk(value.eomodel.get, eoWithDisplayableError).get
      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap + (eoPk -> eoWithDisplayableError)
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))


    case UpdateEOValueForProperty(eo, d2wContext, newEOValue) =>
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get

      D2SpaLogger.logDebug(entityName, "CacheHandler | Update EO Property: for entity " + entityName + " property: " + propertyName + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      D2SpaLogger.logDebug(entityName, "EO: " + eo)
      val updatedEO = eo.copy(values = (eo.values - propertyName) + (propertyName -> newEOValue))

      if (EOValue.isNew(updatedEO.pk)) {
        updated(updatedMemCacheWithEOs(Seq(updatedEO)))
      } else {
        updated(EOCacheUtils.updatedOutOfDBCacheWithEOs(value.eomodel.get, value, Seq(updatedEO)))
      }

    case NewEOWithEOModel(eomodel, d2wContext, actions: List[D2WAction]) =>
      D2SpaLogger.logDebugWithD2WContext(d2wContext,"CacheHandler | NewEOWithEOModel: " + d2wContext)
      val entityName = d2wContext.entityName.get
      // Create the EO and set it in the cache
      val (newValue, newEO) = EOValue.createAndInsertNewObject(value.insertedEOs, entityName)

      D2SpaLogger.logDebug(entityName, "newValue " + newValue)
      D2SpaLogger.logDebug(entityName, "newEO " + newEO)

      //val eo = EOValue.memEOWith(eomodel, entityName, newEO.pk)
      val newD2WContext = d2wContext.copy(eo = Some(newEO))
      updated(updatedMemCache(newValue),
        Effect.action(FireActions(newD2WContext, actions)))

    case NewEOWithEOModelForEdit(entity) =>
      val entityName = entity.name
      D2SpaLogger.logDebug(entityName, "CacheHandler | NewEOWithEOModelForEdit " + entityName)
      val (newValue, newEO) = EOValue.createAndInsertNewObject(value.insertedEOs, entityName)

      D2SpaLogger.logDebug(entityName, "newValue " + newValue)
      D2SpaLogger.logDebug(entityName, "newEO " + newEO)
      //val eo = EOValue.memEOWith(eomodel, entityName, newEO.memID)

      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.edit), eo = Some(newEO))

      updated(updatedMemCache(newValue),
        Effect.action(RegisterPreviousPage(d2wContext)))


  }

}



class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case FetchMenu =>
       D2SpaLogger.logDebug(D2SpaLogger.ALL,"Init Client")
       if (value.isEmpty) {
         D2SpaLogger.logDebug(D2SpaLogger.ALL,"Api get Menus")
         effectOnly(Effect(AjaxClient[Api].getMenus().call().map(SetMenus)))
       } else
         noChange

    case InitMenuAndEO(eo, missingKeys) =>
      effectOnly(Effect(AjaxClient[Api].getMenus().call().map(menus => {
        SetMenusAndEO(menus, eo, missingKeys)
      })))

    case SetMenus(menus) =>
      D2SpaLogger.logDebug(D2SpaLogger.ALL,"Set Menus " + menus)
      updated(Ready(menus), Effect.action(FetchEOModel))


  }

}

