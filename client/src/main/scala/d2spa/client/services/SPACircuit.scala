package d2spa.client.services

import autowire._
import diode.{Effect, _}
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
    case FetchShowD2WDebugButton(pageContext) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"AppConfigurationHandler | FetchShowD2WDebugButton")

      WebSocketClient.send(GetDebugConfiguration(pageContext.d2wContext))
      noChange

    case SetDebugConfiguration(debugConf, d2wContext) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"AppConfigurationHandler | SetShowD2WDebugButton " + debugConf.showD2WDebugButton)
      //val nextAction = if (value.fetchMenus) FetchEOModelAndMenus else FetchEOModel
      updated(value.copy(serverAppConf = DebugConf(debugConf.showD2WDebugButton)), Effect.action(FetchEOModelAndMenus(d2wContext)))


    case SwithDebugMode =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"AppConfigurationHandler | SwithDebugMode")
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
    /*case SocketReady =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"Socket Ready")
      updated(value.copy(socketReady = true), Effect.action(FetchShowD2WDebugButton))*/

    case SetPageForSocketReady(d2wContext) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"AppConfigurationHandler | SetPageForSocketReady")
      updated(value.copy(socketReady = true), Effect.action(FetchShowD2WDebugButton(d2wContext)))

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

  def updatedRuleResultsWithEntityMetaData(pageContext: PageContext, entityMetaData: EntityMetaData) = {
    // convert data from entityMetaData to ruleResults
    val d2wContext = pageContext.d2wContext
    val entityName = d2wContext.entityName.get

    D2SpaLogger.logfinest(entityName,"Register displayNameForEntity " + entityMetaData.displayName + " for task " + d2wContext.task)
    var updatedRuleResults = RuleUtils.registerRuleResult(value, RuleResult(d2wContext, RuleKeys.displayNameForEntity, RuleValue(stringV = Some(entityMetaData.displayName))))
    val displayPropertyKeys = entityMetaData.displayPropertyKeys map (p => p.name)
    updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, RuleResult(d2wContext, RuleKeys.displayPropertyKeys, RuleValue(stringsV = displayPropertyKeys)))

    for (prop <- entityMetaData.displayPropertyKeys) {
      val propertyD2WContext = d2wContext.copy(propertyKey = Some(prop.name))
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

    // 1) NewEO (MenuHandler)
    // 2) Effect: Save DB
    // 3) SavedEO (EOCache)
    // 4) InstallInspectPage (MenuHandler)
    case SaveNewEO(entityName, eo) =>
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SaveNewEO " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      // Will get response with SavingEO
      val d2wContext = D2WContext(entityName = Some(entityName), task =  Some(TaskDefine.inspect))

      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
      val isMetaDataFetched = RulesUtilities.isEmptyRuleRequest(ruleRequest)
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SaveNewEO | isMetaDataFetched " + isMetaDataFetched)
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SaveNewEO | d2wContext " + d2wContext)
      WebSocketClient.send(WebSocketMessages.NewEO(d2wContext, eo, ruleRequest))
      noChange


    case Save(selectedEntityName, eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | Save | eo " + eo)
      val purgedEO = EOValue.purgedEO(eo)

      val d2wContext = D2WContext(entityName = Some(entityName), task =  Some(TaskDefine.inspect))

      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
      val isMetaDataFetched = RulesUtilities.isEmptyRuleRequest(ruleRequest)

      // Update the DB and dispatch the result withing UpdatedEO action
      //
      WebSocketClient.send(WebSocketMessages.UpdateEO(d2wContext, purgedEO, ruleRequest))
      noChange



    case PrepareEODisplayRules(pageContext, cache, needsHydration) =>
      log.finest("RuleResultsHandler | PrepareEODisplayRules | d2wContext " + pageContext)
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)

      if (needsHydration) {
        val eo = pageContext.eo.get
        val eoFault = HydrationUtils.toFault(eo)
        val drySubstrate = DrySubstrate(eo = Some(eoFault))
        val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(value, d2wContext, RuleKeys.displayPropertyKeys)
        //val needsHydration =  HydrationUtils.needsHydration(Some(drySubstrate), displayPropertyKeysRuleResultPot, p.proxy.value.cache, eomodel)

        val hydration = Hydration(drySubstrate, WateringScope(ruleResult = displayPropertyKeysRuleResultPot))
        WebSocketClient.send(WebSocketMessages.Hydrate(Some(d2wContext), hydration, Some(ruleRequest)))
        noChange

      } else {
        log.finest("RuleResultsHandler | PrepareEODisplayRules | new eo ")

        if (RulesUtilities.isEmptyRuleRequest(ruleRequest)) {
          effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContext)))
        } else {
          WebSocketClient.send(WebSocketMessages.AppInitMsgIn(ruleRequest, pageContext.eo))
          noChange
        }
      }


    case GetMetaDataForSetPage(pageContext) =>
      val d2wContext = pageContext.d2wContext

      log.finest("RuleResultsHandler | GetMetaDataForSetPage: ")
      //val taskFault: TaskFault = rulesCon match {case taskFault: TaskFault => taskFault}
      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
      log.finest("RuleResultsHandler | GetMetaDataForSetPage | ruleRequest " + ruleRequest)
      val isEmptyRuleRequest = RulesUtilities.isEmptyRuleRequest(ruleRequest)
      if (isEmptyRuleRequest) {
        log.finest("PreviousPageHandler | GetMetaDataForSetPage | no rules to fetch " + d2wContext.entityName)
        effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContext)))
      } else {
        WebSocketClient.send(WebSocketMessages.AppInitMsgIn(ruleRequest, pageContext.eo)) // reply with RuleResults and then action SetJustRuleResults
        noChange
      }


    case SetMetaData(d2wContext, ruleResultsOpt: Option[List[RuleResult]]) =>
      val entityName = d2wContext.entityName.get
      //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData " + entityMetaData)
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData ")
      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(RegisterPreviousPageAndSetPage(pageContext)))

        case None =>
          effectOnly( Effect.action(RegisterPreviousPageAndSetPage(pageContext)))
      }

    case SetRulesForPrepareEO(d2wContext, ruleResultsOpt: Option[List[RuleResult]], eoOpt) =>

      val entityName = d2wContext.entityName.get
      //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData " + entityMetaData)
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetMetaData ")
      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      val pageContextUpdated = pageContext.copy(eo = eoOpt)
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(RegisterPreviousPageAndSetPage(pageContextUpdated)))

        case None =>
          effectOnly( Effect.action(RegisterPreviousPageAndSetPage(pageContextUpdated)))
      }


    //case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
    //                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
    //case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer

    // many rules
   /* obsolete ??
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
*/

    case SetJustRuleResults(ruleResults) =>
      log.finest("RuleResultsHandler | SetJustRuleResults | ruleResults " + ruleResults)
      val newRuleResults = updatedRuleResults(value,ruleResults)
      updated(newRuleResults)


    case SearchResultWithRuleResults(fs, eos, ruleResultsOpt) =>
      log.finest("RuleResultsHandler | SearchResultWithRuleResults | ruleResults " + ruleResultsOpt)
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(RegisterSearchResults(fs, eos)))
        case None =>
          effectOnly(Effect.action(RegisterSearchResults(fs, eos)))
      }


    case SetPreviousWithResults(ruleResults, d2wContext) =>
      log.finest("RuleResultsHandler | SetPreviousWithResults | ruleResults " + ruleResults)
      val newRuleResults = updatedRuleResults(value,ruleResults)
      updated(newRuleResults, Effect.action(RegisterPreviousPageAndSetPage(d2wContext)))

    case EditEOWithResults(fromTask, eo, d2wContext, ruleResultsOpt: Option[List[RuleResult]]) =>
      log.finest("RuleResultsHandler | EditEOWitResults | ruleResults " + ruleResultsOpt)
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(EditEO(d2wContext)))
        case None =>
          effectOnly(Effect.action(EditEO(d2wContext)))
      }

    case SavedEOWithResults(fromTask, eo, d2wContext, ruleResultsOpt: Option[List[RuleResult]]) =>
      log.finest("RuleResultsHandler | EditEOWitResults | ruleResults " + ruleResultsOpt)
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(SavedEO(d2wContext)))
        case None =>
          effectOnly(Effect.action(SavedEO(d2wContext)))
      }

    case SearchResult(fs, eoses) =>
      log.finest("RuleResultsHandler | SearchResult " + eoses.size)
      val entityName = EOFetchSpecification.entityName(fs)

      eoses.length match {
        case x if x == 1 =>
          val eo = eoses(0)
          val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.inspect))
          val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
          val pageContextUpdated = pageContext.copy(eo = Some(eo))


          effectOnly(Effect.action(CacheForPrepareEODisplay(eo, pageContextUpdated)))

        case _ =>
          val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list))
          val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
          if (RulesUtilities.isEmptyRuleRequest(ruleRequest)) {
            effectOnly(Effect.action(SearchResultWithRuleResults(fs, eoses, None)))

          } else {
            log.finest("RuleResultsHandler | ruleRequest " + ruleRequest)
            WebSocketClient.send(WebSocketMessages.RuleRequestForSearchResult(fs, eoses, ruleRequest))
            noChange
          }
      }



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

  /*def removeEOFromDBCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    EOCacheUtils.removeEOFromCache(eo,  eos)
  }

  def removeEOFromMemCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    EOCacheUtils.removeEOFromCache(eo, eos)
  }*/



  override def handle = {


    case FetchEOModelAndMenus(d2wContext) =>
      log.finest("EOCacheHandler | FetchEOModelAndMenus")
      WebSocketClient.send(WebSocketMessages.FetchEOModel(d2wContext))
      noChange

    case SetEOModelThenFetchMenu(eomodel, d2wContext) =>
      log.finest("EOCacheHandler | SetEOModelThenFetchMenu")
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(FetchMenu(d2wContext)))


    /*case SetEOModel(eomodel) =>
      D2SpaLogger.logfinest(D2SpaLogger.ALL,"FetchEOModel set eomodel ")
      val updatedModel = value.copy(eomodel = Ready(eomodel))
      updated(updatedModel, Effect.action(InitAppSpecificClient))*/




    /*case NewEOWithEntityNameForEdit(selectedEntityName) =>
      D2SpaLogger.logfinest(entityName,"EOModelHandler | NewEOWithEntityNameForEdit " + selectedEntityName)
      effectOnly(
        Effect.action(NewEOWithEOModelForEdit(value.get, selectedEntityName)) // from edit ?
      )*/

    case EditEO(pageContext) =>
      val d2wContext = pageContext.d2wContext
      val eo = pageContext.eo.get
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | EditEO | register faulty eo  " + eo)

      // Adjust the db cache
      val newCache = updatedMemCacheWithEOs(entityName, Seq(eo))
      updated(
        newCache
      )
    case  HydrationRequest(hydration: Hydration, ruleRequestOpt: Option[RuleRequest]) =>
      log.finest("EOCacheHandler | HydrationRequest")
      WebSocketClient.send(WebSocketMessages.Hydrate(None, hydration, ruleRequestOpt))
      noChange



    // EO goes from inserted EO to db eos
    case SavedEO(pageContext) =>
      val d2wContext = pageContext.d2wContext
      val eo = pageContext.eo.get
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | entityName  " + entityName)

      val eomodel = value.eomodel.get
      D2SpaLogger.logfinest(entityName," v " + eo)
      val isNewEO = EOValue.isNewEO(eo)
      val updatedEO = if (isNewEO) {
        val pk = EOValue.pk(eomodel, eo)
        D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | pk out of EO  " + pk)

        eo.copy(pk = pk.get)
      } else eo

      val updatedPageContext = pageContext.copy(eo = Some(updatedEO))
      D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | updatedCachesForSavedEO  " + updatedEO + " eo " + eo)
      val newCache = EOCacheUtils.updatedCachesForSavedEO(value, updatedEO, Some(eo))
      D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | updated cache " + newCache)
      D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO update cache, call action Register with context " + updatedPageContext)
      updated(
        newCache,
          Effect.action(RegisterPreviousPageAndSetPage(updatedPageContext))
      )

    case CacheForPrepareEODisplay(eo: EO, pageContext: PageContext) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | CacheForPrepareEODisplay | entityName  " + entityName)
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eo), entityName)
      updated(newCache,Effect.action(PrepareEODisplayRules(pageContext, value, false)))


    case RegisterAndPrepareEODisplay(eo, pageContext) =>
      val entityName = eo.entityName
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eo), entityName)
      updated(newCache,Effect.action(PrepareEODisplay(pageContext)))


    case SavingEO(d2wContext: D2WContext, eo: EO, ruleResults: Option[List[RuleResult]]) =>
      D2SpaLogger.logfinest(eo.entityName,"CacheHandler | SavingEO " + eo)
      D2SpaLogger.logfinest(eo.entityName,"CacheHandler | SavingEO | cache " + value)
      D2SpaLogger.logfinest(eo.entityName,"CacheHandler | SavingEO | d2wContext " + d2wContext)
      D2SpaLogger.logfinest(eo.entityName,"CacheHandler | SavingEO | ruleResults " + ruleResults)
      // Update the DB and dispatch the result withing UpdatedEO action
      val onError = eo.validationError.isDefined

      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      val updatedPageContext = pageContext.copy(eo = Some(eo))


      val action = if (onError) {
          // TODO implement it
          EditEOWithResults("edit", eo, updatedPageContext, ruleResults)

      } else {
          SavedEOWithResults("edit", eo, updatedPageContext, ruleResults)
      }
      effectOnly(Effect.action(action))



    // EO from router is a pk mainly (in an EO shell)
    // Different case may occur:
    // 1) pk is none, New EO
    //    - create a new EO, register it in the cache
    // 2) pk is Some, existing EO
    //    2 cases:
    //      2.1) eo in cache
    //      2.2) eo not in cache
    //          - Fetch it
    //             2 cases:
    //                 2.2.1) EO found in DB
    //                     - register it
    //                 2.2.2) EO not found in DB --> error
    // After, if successful, we continue with rules
    case PrepareEODisplay(pageContext) =>
      val d2wContext = pageContext.d2wContext

      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName, "CacheHandler | PrepareEODisplay | called, cache " + value)
      val eoOpt = pageContext.eo
      eoOpt match {
        case Some(eoRef) =>
          // case 2: Existing EO
          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(value, entityName, eoRef)
          log.finest("CacheHandler | PrepareEODisplay | Existing EO from cache: " + eoOpt)

          eoOpt match {
            case Some(existingEO) =>
              // case 2.1: Existing EO in cache
              val pageContextUpdated = pageContext.copy(eo = Some(existingEO))

              effectOnly(Effect.action(PrepareEODisplayRules(pageContextUpdated, value, true)))
            case None =>
              // case 2.2: Existing EO not in cache => fetch it
              val pageContextUpdated = RuleUtils.pageContextWithD2WContext(d2wContext)

              effectOnly(Effect.action(PrepareEODisplayRules(pageContextUpdated, value , true)))
          }
        case None =>
          // case 1: new EO
          log.finest("CacheHandler | PrepareEODisplay | New EO " + value)
          val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(value, entityName)
          D2SpaLogger.logfinest(entityName, "newEO " + newEO)
          val pageContextUpdated = pageContext.copy(eo = Some(newEO))

          // We may need to fetch rules
          updated(newCache,Effect.action(PrepareEODisplayRules(pageContextUpdated, value,false)))
      }
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
*/
    case FetchedObjectsForEntity(entityName, eos, ruleResultsOpt) =>
      log.finest("CacheHandler | FetchedObjectsForEntity eoses " + eos)
      val entity = EOModelUtils.entityNamed(value.eomodel.get,entityName).get
      ruleResultsOpt match {
        case Some(ruleResults) =>
          updated(
            EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos.toList, entityName),
            Effect.action(SetJustRuleResults(ruleResults))
          )

        case None =>
          updated(
            EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos.toList, entityName)
          )

      }


    case RegisterPreviousPageAndSetPageRemoveMemEO(pageContext, eo) =>
      val d2wContext = pageContext.d2wContext
      log.finest("CacheHandler | RegisterPreviousPageAndSetPageRemoveMemEO: " + d2wContext.entityName)
      updated(
        EOCacheUtils.updatedMemCacheByRemovingEO(value, eo),
        Effect.action(RegisterPreviousPageAndSetPagePure(pageContext))
      )


    case CompletedEO(d2wContextOpt, eos, ruleResultsOpt) =>
      log.finest("CacheHandler | CompletedEOs  " + eos)
      //val entity = EOModelUtils.entityNamed(value.eomodel.get,entityName).get

      d2wContextOpt match {
        case Some(d2wContext) =>
          val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
          val eo = eos.head
          val pageContextUpdated = pageContext.copy(eo = Some(eo))

          val entityName = d2wContext.entityName.get

          val isNewEO = EOValue.isNewEO(eo)
          if (isNewEO) {
            ruleResultsOpt match {
              case Some(ruleResults) =>
                effectOnly(Effect.action(SetPreviousWithResults(ruleResults, pageContextUpdated)))
              case None =>
                effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContextUpdated)))
            }
          } else {
            ruleResultsOpt match {
              case Some(ruleResults) =>
                updated(
                  EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eo), entityName),
                  Effect.action(SetPreviousWithResults(ruleResults, pageContextUpdated))
                )

              case None =>
                updated(
                  EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eo), entityName),
                  Effect.action(RegisterPreviousPageAndSetPage(pageContextUpdated))
                )
            }
          }

        case None =>
          eos.length match {
            case x if x == 0 =>
              ruleResultsOpt match {
                case Some(ruleResults) =>
                  effectOnly(Effect.action(SetJustRuleResults(ruleResults)))
                case None =>
                  noChange
              }
            case _ =>
              val eo = eos.head
              val entityName = eo.entityName
              val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos, entityName)

              ruleResultsOpt match {
                case Some(ruleResults) =>
                  updated(newCache,
                    Effect.action(SetJustRuleResults(ruleResults)))
                case None =>
                  updated(newCache)

              }
          }

      }



    case RegisterSearchResults(fs, eoses) =>
      val entityName = EOFetchSpecification.entityName(fs)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults length " + eoses.length)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults before cache " + value)

      val action = eoses.length match {
        case x if x == 1 => {
          val eo = eoses.head
          val d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.inspect))
          val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
          val pageContextUpdated = pageContext.copy(eo = Some(eo))
          PrepareEODisplay(pageContextUpdated)
        }
        case _ => ShowResults(fs)
      }

      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults | action " + action)
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eoses.toList, entityName)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults | newCache " + newCache)



      updated(
        newCache,
        Effect.action(action)
      )


    case DeleteEOFromList(eo) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | DeleteEOFromList " + eo)

      WebSocketClient.send(WebSocketMessages.DeleteEOMsgIn(eo))
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


    case UpdateEOValueForProperty(eo, pageContext, newEOValue) =>
      println("UpdateEOValueForProperty")
      val d2wContext = pageContext.d2wContext
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

    case RegisteredExistingEO(pageContext) =>
      D2SpaLogger.logDebugWithD2WContext(pageContext,"CacheHandler | RegisteredExistingEO: " + pageContext)
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      // Create the EO and set it in the cache

      println("entityName " + entityName)
      val newCache = EOCacheUtils.updatedDBCacheWithEO(value, pageContext.eo.get)
      println("newCache " + newCache)

      updated(newCache,Effect.action(RegisterPreviousPage(pageContext)))



  }

}


class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {

    // server response will come with SetMenus
    case FetchMenu(d2wContext) =>
      log.finest("Init Client")
      if (value.isEmpty) {
        log.finest("Api get Menus")
        WebSocketClient.send(FetchMenus(d2wContext))
      }
      noChange


    case SetMenus(menus, d2wContext) =>
      log.finest("Set Menus " + menus)
      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      updated(Ready(menus), Effect.action(InitAppSpecificClient(pageContext)))


  }

}

