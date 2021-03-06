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
import d2spa.client.{AppModel, InitAppSpecificClient, _}
import d2spa.client.logger._
import d2spa.shared.WebSocketMessages._

import scala.collection.immutable.Set


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

    case  SendRuleRequest (ruleRequest) =>
      log.finest("PreviousPageHandler | SendRuleRequest | ruleRequest: " + ruleRequest)
      WebSocketClient.send(WebSocketMessages.ExecuteRuleRequest(ruleRequest))
      noChange

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
      WebSocketClient.send(WebSocketMessages.NewEO(d2wContext, List(eo), ruleRequest))
      noChange


    case Save(selectedEntityName, eoContaining) =>
      val eo = eoContaining.eo
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"RuleResultsHandler | Save | eo " + eo)
      val purgedEO = EOValue.purgedEO(eoContaining)

      val d2wContext = D2WContext(entityName = Some(entityName), task =  Some(TaskDefine.inspect))

      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
      val isMetaDataFetched = RulesUtilities.isEmptyRuleRequest(ruleRequest)

      // Update the DB and dispatch the result withing UpdatedEO action
      //
      WebSocketClient.send(WebSocketMessages.UpdateEO(d2wContext, List(purgedEO), ruleRequest))
      noChange


      // With no argument: never called !
    case InitAppSpecificClient =>
      //effectOnly(Effect(AjaxClient[Api].searchAll(EOFetchAll("Customer")).call().map(AllCustomerSearchResult(_))))
      val d2wContext = D2WContext(Some("Customer"), Some(TaskDefine.list), None, None)
      val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(value, d2wContext, RuleKeys.displayPropertyKeys)
      val wateringScope = WateringScope(ruleResult = displayPropertyKeysRuleResultPot)
      val drySubstrate = DrySubstrate(fetchSpecification = Some(EOFetchAll("Customer")))
      val hydration = Hydration(drySubstrate, wateringScope)
      WebSocketClient.send(WebSocketMessages.Hydrate(Some(d2wContext), hydration, None))
      noChange


    case PrepareEODisplayRules(pageContext, cache, needsHydration) =>
      log.finest("RuleResultsHandler | PrepareEODisplayRules | d2wContext " + pageContext)
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)

      if (needsHydration) {
        val eoContaining = pageContext.eo.get
        val eo = eoContaining.eo
        val eoFault = HydrationUtils.toFault(eoContaining)
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

    case SearchHydration(fs) =>
      val entityName = EOFetchSpecification.entityName(fs)
      entityName match {
        // case "Customer" =>
          //effectOnly( Effect.action(SearchInMemory(fs)))
        //  noChange
        case _ =>
          val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list))
          val drySubstrate = DrySubstrate(fetchSpecification = Some(fs))
          val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(value, d2wContext, RuleKeys.displayPropertyKeys)
          val wateringScope = WateringScope(ruleResult = displayPropertyKeysRuleResultPot)
          val hydration = Hydration(drySubstrate, wateringScope)
          val ruleRequest = RuleUtils.metaDataRuleRequest(value, d2wContext)
          WebSocketClient.send(WebSocketMessages.Hydrate(Some(d2wContext), hydration, Some(ruleRequest)))
          noChange

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

       D2SpaLogger.logfinestWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults")
       D2SpaLogger.logfinestWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | ruleResults: " + ruleResults)
       D2SpaLogger.logfinestWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | actions: " + actions)
       D2SpaLogger.logfinestWithD2WContext(d2wContext,"RuleResultsHandler | SetRuleResults | d2wContext: " + d2wContext)
       //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetRuleResults " + ruleResults + " in d2w context " + d2wContext)
       //D2SpaLogger.logfinest(entityName,"RuleResultsHandler | SetRuleResults | actions: " + actions)
       var updatedRuleResults = value
       for (ruleResult <- ruleResults) {
         updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, ruleResult)
       }
       updated(updatedRuleResults, Effect.action(FireActions(d2wContext, actions)))
 */

    case SetJustRuleResults(ruleResults) =>
      //log.finest("RuleResultsHandler | SetJustRuleResults | ruleResults " + ruleResults)
      log.finest("RuleResultsHandler | SetJustRuleResults | ruleResults ")
      val newRuleResults = updatedRuleResults(value,ruleResults)
      updated(newRuleResults)


    case SearchResultWithRuleResults(fs, eos, ruleResultsOpt) =>
      //log.finest("RuleResultsHandler | SearchResultWithRuleResults | ruleResults " + ruleResultsOpt)
      log.finest("RuleResultsHandler | SearchResultWithRuleResults | ruleResults ")
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(RegisterSearchResults(fs, eos)))
        case None =>
          effectOnly(Effect.action(RegisterSearchResults(fs, eos)))
      }


    case SetPreviousWithResults(ruleResults, d2wContext) =>
      //log.finest("RuleResultsHandler | SetPreviousWithResults | ruleResults " + ruleResults)
      log.finest("RuleResultsHandler | SetPreviousWithResults | ruleResults ")
      val newRuleResults = updatedRuleResults(value,ruleResults)
      updated(newRuleResults, Effect.action(RegisterPreviousPageAndSetPage(d2wContext)))

    case EditEOWithResults(fromTask, eo, d2wContext, ruleResultsOpt: Option[List[RuleResult]]) =>
      //log.finest("RuleResultsHandler | EditEOWitResults | ruleResults " + ruleResultsOpt)
      log.finest("RuleResultsHandler | EditEOWitResults | ruleResults ")
      ruleResultsOpt match {
        case Some(ruleResults) =>
          val newRuleResults = updatedRuleResults(value,ruleResults)
          updated(newRuleResults, Effect.action(EditEO(d2wContext)))
        case None =>
          effectOnly(Effect.action(EditEO(d2wContext)))
      }

    case SavedEOWithResults(fromTask, eo, d2wContext, ruleResultsOpt: Option[List[RuleResult]]) =>
      //log.finest("RuleResultsHandler | EditEOWitResults | ruleResults " + ruleResultsOpt)
      log.finest("RuleResultsHandler | EditEOWitResults | ruleResults ")
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




  def updatedMemCacheWithEOs(entityName: String, eos: Seq[EOContaining]): EOCache = {
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

  def registerEosAndRules(pageContext: PageContext, eos: List[EOContaining], ruleResultsOpt: Option[List[RuleResult]]) = {
    log.finest("EOCacheHandler | registerEosAndRules " + eos.size + " rule results " + ruleResultsOpt.isDefined)
    if (eos.size == 1) {
      log.finest("EOCacheHandler | registerEosAndRules | eos  " + eos)

    }
    val entityName = pageContext.d2wContext.entityName.get
    ruleResultsOpt match {
      case Some(ruleResults) =>
        updated(
          EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos, entityName),
          Effect.action(SetPreviousWithResults(ruleResults, pageContext))
        )

      case None =>
        updated(
          EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos, entityName),
          Effect.action(RegisterPreviousPageAndSetPage(pageContext))
        )
    }
  }

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
      val eoContaining = pageContext.eo.get
      val eo = eoContaining.eo
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | EditEO | register faulty eo  " + eo)

      // Adjust the db cache
      val newCache = updatedMemCacheWithEOs(entityName, Seq(eoContaining))
      updated(
        newCache
      )
    case HydrationRequest(hydration: Hydration, ruleRequestOpt: Option[RuleRequest]) =>
      log.finest("EOCacheHandler | HydrationRequest")
      WebSocketClient.send(WebSocketMessages.Hydrate(None, hydration, ruleRequestOpt))
      noChange


    case SearchInMemory(fs) =>
      noChange

    // EO goes from inserted EO to db eos
    case SavedEO(pageContext) =>
      val d2wContext = pageContext.d2wContext
      val eoContaining = pageContext.eo.get
      val eo = eoContaining.eo
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | entityName  " + entityName)

      val eomodel = value.eomodel.get
      D2SpaLogger.logfinest(entityName, " v " + eo)
      val isNewEO = EOValue.isNewEO(eoContaining)
      val updatedEO: EOContaining = if (isNewEO) {
        val pk = EOValue.pk(eomodel, eoContaining)
        D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | pk out of EO  " + pk)

        val uEO = eo.copy(pk = pk.get)
        EOContaining.updateEOContainingEO(eoContaining, uEO)
      } else eoContaining

      val updatedPageContext = pageContext.copy(eo = Some(updatedEO))
      D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | updatedCachesForSavedEO  " + updatedEO + " eo " + eo)
      val newCache = EOCacheUtils.updatedCachesForSavedEO(value, updatedEO, Some(eoContaining))
      D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | updated cache " + newCache)
      D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO update cache, call action Register with context " + updatedPageContext)
      updated(
        newCache,
        Effect.action(RegisterPreviousPageAndSetPage(updatedPageContext))
      )

    case CacheForPrepareEODisplay(eo: EO, pageContext: PageContext) =>
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | CacheForPrepareEODisplay | entityName  " + entityName)
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eo), entityName)
      updated(newCache, Effect.action(PrepareEODisplayRules(pageContext, value, false)))


    case RegisterAndPrepareEODisplay(eoContaining, pageContext) =>
      val eo = eoContaining.eo
      val entityName = eo.entityName
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, List(eoContaining), entityName)
      updated(newCache, Effect.action(PrepareEODisplay(pageContext)))


    case SavingEO(d2wContext: D2WContext, eoContaining, ruleResults: Option[List[RuleResult]]) =>
      val eo = eoContaining.eo
      D2SpaLogger.logfinest(eo.entityName, "CacheHandler | SavingEO " + eoContaining)
      D2SpaLogger.logfinest(eo.entityName, "CacheHandler | SavingEO | cache " + value)
      D2SpaLogger.logfinest(eo.entityName, "CacheHandler | SavingEO | d2wContext " + d2wContext)
      D2SpaLogger.logfinest(eo.entityName, "CacheHandler | SavingEO | ruleResults " + ruleResults)
      // Update the DB and dispatch the result withing UpdatedEO action
      val onError = eo.validationError.isDefined

      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      val updatedPageContext = pageContext.copy(eo = Some(eoContaining))


      val action = if (onError) {
        // TODO implement it
        EditEOWithResults("edit", eoContaining, updatedPageContext, ruleResults)

      } else {
        SavedEOWithResults("edit", eoContaining, updatedPageContext, ruleResults)
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
      //D2SpaLogger.logfinest(entityName, "CacheHandler | PrepareEODisplay | called, cache " + value)
      D2SpaLogger.logfinest(entityName, "CacheHandler | PrepareEODisplay")
      val eoContainingOpt = pageContext.eo
      eoContainingOpt match {
        case Some(eoContaining) =>
          val eo = eoContaining.eo
          // case 2: Existing EO
          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(value, entityName, eoContaining)
          //log.finest("CacheHandler | PrepareEODisplay | Existing EO from cache: " + eoOpt)

          eoOpt match {
            case Some(existingEO) =>
              // case 2.1: Existing EO in cache
              val pageContextUpdated = pageContext.copy(eo = Some(existingEO))
              //val isCustomer = entityName.equals("Customer")
              val isCustomer = false
              if (isCustomer) {
                effectOnly(Effect.action(InspectEO(TaskDefine.list,existingEO)))

              } else {
                effectOnly(Effect.action(PrepareEODisplayRules(pageContextUpdated, value, true)))
              }
            case None =>
              // case 2.2: Existing EO not in cache => fetch it
              val pageContextUpdated = RuleUtils.pageContextWithD2WContext(d2wContext)

              effectOnly(Effect.action(PrepareEODisplayRules(pageContextUpdated, value, true)))
          }
        case None =>
          // case 1: new EO
          log.finest("CacheHandler | PrepareEODisplay | New EO " + value)
          val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(value, entityName)
          D2SpaLogger.logfinest(entityName, "newEO " + newEO)
          val pageContextUpdated = pageContext.copy(eo = Some(newEO))

          // We may need to fetch rules
          updated(newCache, Effect.action(PrepareEODisplayRules(pageContextUpdated, value, false)))
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
      val entity = EOModelUtils.entityNamed(value.eomodel.get, entityName).get
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


    case CompletedEO(d2wContextOpt, hydration, eos, ruleResultsOpt) => {
      effectOnly(Effect.action(CompletedEOProcessing(d2wContextOpt, Some(hydration), eos, ruleResultsOpt)))
    }
    case CompletedEOProcessing(d2wContextOpt, hydrationOpt, eos, ruleResultsOpt) =>
      log.finest("CacheHandler | CompletedEOProcessing  " + eos.size + " rule results " + ruleResultsOpt.isDefined)
      //val entity = EOModelUtils.entityNamed(value.eomodel.get,entityName).get
      log.finest("CacheHandler | CompletedEOProcessing | d2wContextOpt  " + d2wContextOpt)

      d2wContextOpt match {
        case Some(d2wContext) =>
          val entityName = d2wContext.entityName.get

          val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
          val task = d2wContext.task.get
          log.finest("CacheHandler | CompletedEOs | d2wContext | task " + task)

          task match {
            case TaskDefine.list =>
              hydrationOpt match {
                case Some(hydration) =>
                  val fs = hydration.drySubstrate.fetchSpecification.get
                  val dataRep = DataRep (fetchSpecification = Some(fs))
                  val pageContextUpdated = pageContext.copy(dataRep = Some(dataRep))
                  registerEosAndRules(pageContextUpdated, eos, ruleResultsOpt)
                case None =>
                  noChange
              }

            case _ =>
              val eo = eos.head
              val pageContextUpdated = pageContext.copy(eo = Some(eo))


              val isNewEO = EOValue.isNewEO(eo)
              if (isNewEO) {
                ruleResultsOpt match {
                  case Some(ruleResults) =>
                    effectOnly(Effect.action(SetPreviousWithResults(ruleResults, pageContextUpdated)))
                  case None =>
                    effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContextUpdated)))
                }
              } else {
                registerEosAndRules(pageContextUpdated, eos, ruleResultsOpt)

              }

          }


        case None =>
          log.finest("CacheHandler | CompletedEOProcessing | background update")
          eos.length match {
            case x if x == 0 =>
              ruleResultsOpt match {
                case Some(ruleResults) =>
                  effectOnly(Effect.action(SetJustRuleResults(ruleResults)))
                case None =>
                  noChange
              }
            case _ =>
              val eoContaining = eos.head
              val eo = eoContaining.eo
              val entityName = eo.entityName
              val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eos, entityName)
              log.finest("CacheHandler | CompletedEOProcessing | newCache ")

              ruleResultsOpt match {
                case Some(ruleResults) =>
                  updated(newCache,
                    Effect.action(SetJustRuleResults(ruleResults)))
                case None =>
                  updated(newCache)

              }
          }

      }

    case RegisterEO(eo) =>
      val newCache = EOCacheUtils.updatedDBCacheWithEO(value, eo)
      updated(newCache)

    case RegisterSearchResults(fs, eoses) =>
      val entityName = EOFetchSpecification.entityName(fs)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults length " + eoses.length)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults before cache " + value)

      val action = eoses.length match {
        case x if x == 1 => {
          val eoContaining = eoses.head
          val eo = eoContaining.eo
          val d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.inspect))
          val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
          val pageContextUpdated = pageContext.copy(eo = Some(eoContaining))
          PrepareEODisplay(pageContextUpdated)
        }
        case _ => ShowFetchResults(fs)
      }

      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults | action " + action)
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(value, eoses.toList, entityName)
      D2SpaLogger.logfinest(entityName,"CacheHandler | RegisterSearchResults | newCache " + newCache)



      updated(
        newCache,
        Effect.action(action)
      )


    case DeleteEOFromList(eoContaining) =>
      val eo = eoContaining.eo
      val entityName = eo.entityName
      D2SpaLogger.logfinest(entityName,"CacheHandler | DeleteEOFromList " + eo)

      WebSocketClient.send(WebSocketMessages.DeleteEOMsgIn(eoContaining))
      noChange


    case DeletingEO(eoContaining) =>
      log.finest("CacheHandler | DeletingEO " + eoContaining)

      /*val eos = value.get
      val newEos = eos.filterNot(o => {o.id.equals(eo.id)})
      updated(Ready(newEos))*/
      val deletedEO = eoContaining.eo

      val onError = deletedEO.validationError.isDefined
      if (onError) {
        log.finest("Deleted EO error " + deletedEO.validationError)
        effectOnly(Effect.action(UpdateEOsForEOOnError(eoContaining)))

      } else {
        log.finest("Deleted EO action ")

        effectOnly(Effect.action(DeletedEO(eoContaining)))
      }


    case DeletedEO(eoContaining) =>
      val deletedEO = eoContaining.eo
      val entityName = deletedEO.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | Deleted EO " + eoContaining)

      val newValue = EOCacheUtils.updatedDBCacheByDeletingEO(value,eoContaining)
      updated(newValue)



    // set error on eo
    case UpdateEOsForEOOnError(eoContainingOnError) =>
      val eoOnError = eoContainingOnError.eo
      val entityName = eoOnError.entityName
      D2SpaLogger.logfinest(entityName, "CacheHandler | UpdateEOsForEOOnError " + eoContainingOnError)
      val eo = EOValue.escapeValidationError(eoContainingOnError)
      val newValue = EOCacheUtils.updatedDBCacheWithEO(value,eo)
      updated(newValue)


    case UpdateEOValueForProperty(eoContaining, pageContext, newEOValue) =>
      println("CacheHandler | UpdateEOValueForProperty")
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val eo = eoContaining.eo

      D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: for entity " + entityName + " property: " + propertyName + " " + newEOValue)

      D2SpaLogger.logfinest(entityName, "EO: " + eoContaining)
      val noValidationErrorEO = eo.copy(validationError = None)
      val noValidationErrorEOContaining = EOContaining.updateEOContainingEO(eoContaining,noValidationErrorEO)
      val updatedEO = EOValue.takeValueForKey(noValidationErrorEOContaining, newEOValue, propertyName)
      D2SpaLogger.logfinest(entityName, "CacheHandler | Update EO Property: updatedEO " + updatedEO)

      if (EOValue.isNew(updatedEO.eo.pk)) {
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

class PreviousPageHandler[M](modelRW: ModelRW[M, Option[PageContext]]) extends ActionHandler(modelRW) {

  def stackD2WContext(d2WContext: PageContext) : PageContext = {
    value match {
      case Some(currentPage) => {
        d2WContext.copy(previousTask = value)
      }
      case _ => d2WContext
    }
  }

  override def handle = {


    case ShowFetchResults(fs) =>
      log.finest("PreviousPageHandler | ShowResults: " + fs)
      val entityName = EOFetchSpecification.entityName(fs)
      val pageContext = PageContext (
        dataRep = Some(DataRep(Some(fs))),
        d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list)))
      effectOnly(Effect.action(RegisterPreviousPageAndSetPage(pageContext)))





    case UpdateCurrentContextWithEO(eo) =>
      val d2wContext = value.get.d2wContext
      val pageContext = RuleUtils.pageContextWithD2WContext(d2wContext)
      val pageContextUpdated = pageContext.copy(eo = Some(eo))
      updated(Some(pageContextUpdated))


    case RegisterPreviousPageAndSetPage(pageContext) =>
      val d2wContext = pageContext.d2wContext

      log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage: " + d2wContext.entityName)
      val newEOToBeRemovedFromCache = value match {
        case Some(currentPage) =>
          val currentPageD2WContext = currentPage.d2wContext
          if (currentPageD2WContext.task.get.equals(TaskDefine.edit)) {
            val eoOpt = currentPage.eo
            eoOpt match {
              case Some(eo) =>
                val isNewEO = EOValue.isNewEO(eo)
                log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage | eo " + eo)
                log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage | isNewEO " + isNewEO)
                if (isNewEO) {
                  Some(eo)
                } else None
              case None =>
                None
            }
          } else None
        case None => None
      }
      newEOToBeRemovedFromCache match {
        case Some(eo) =>
          effectOnly(Effect.action(RegisterPreviousPageAndSetPageRemoveMemEO(pageContext, eo)))

        case None =>
          effectOnly(Effect.action(RegisterPreviousPageAndSetPagePure(pageContext)))
      }


    case RegisterPreviousPageAndSetPagePure(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | RegisterPreviousPageAndSetPage for d2wContext: " + stack)

      updated(Some(stack),Effect.action(SetPage(stack)))


    case RegisterPreviousPage(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | RegisterPreviousPage for d2wContext: " + stack)

      updated(Some(stack))

    case PrepareEditPage(d2wContext) =>
      val  stack = stackD2WContext(d2wContext)
      log.finest("PreviousPageHandler | PrepareEditPage for d2wContext: " + stack)

      updated(Some(stack),Effect.action(PrepareEODisplay(stack)))

    case SetPage(d2WContext) =>
      log.finest("PreviousPageHandler | SetPage for d2wContext: " + d2WContext)
      effectOnly(
        Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(d2WContext))
      )


    case SetPreviousPage =>
      log.finest("PreviousPageHandler | SetPreviousPage to: " + value)
      value match {
        case Some(currentPage) => {
          currentPage.previousTask match {
            case Some(previousPage) =>
              updated(
                Some(previousPage),
                Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(previousPage))
              )
            case _ => noChange
          }
        }
        case _ => noChange
      }

    case UpdateQueryProperty(entityName, queryValue) =>
      log.finest("PreviousPageHandler | UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue + " value " + value)
      val pageContext = value.get
      val newD2wContext = D2WContextUtils.pageContextUpdatedWithQueryValue(pageContext,queryValue)
      log.finest("PreviousPageHandler | newD2wContext: " + newD2wContext)
      updated(Some(newD2wContext))

    case ClearQueryProperty(entityName, propertyName, operator) =>
      log.finest("PreviousPageHandler | ClearQueryProperty: for entity " + entityName + " and property " + propertyName)
      val pageContext = value.get
      val newPageContext = D2WContextUtils.updatePageContextByClearingKeyForOperator(pageContext,propertyName, operator)
      updated(Some(newPageContext))

    case SearchAction(entityName) =>
      log.finest("PreviousPageHandler | PrepareSearchForServer | entityName: " + entityName)
      val fs: EOFetchSpecification = value match {
        case Some(d2wContext) =>
          val qualifierOpt = QueryValue.qualifierFromQueryValues(d2wContext.queryValues)
          log.finest("SPACircuit | Search(" + entityName + ") | qualifierOpt: " + qualifierOpt)

          qualifierOpt match {
            // TODO manage sort ordering
            case Some(qualifier) => EOQualifiedFetch(entityName,qualifier,List())
            case _ =>   EOFetchAll(entityName)
          }
        case _ => EOFetchAll(entityName)  // shouldn't come here because the query page initialized it
      }
      log.finest("PreviousPageHandler | Search | " + entityName + " query with fs " + fs)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)

      //val  stack = stackD2WContext(d2wContext)
      //log.finest("PreviousPageHandler | Search | Register Previous " + stack)

      effectOnly(Effect.action(SearchHydration(fs)))

    //updated(
    // change context to inspect
    //  Some(stack)
    //)

  }
}

