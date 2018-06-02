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

import d2spa.shared.{Menus, EntityMetaData, PropertyMetaInfo, EO, EOValue}

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
    new DebugConfigurationHandler(zoomTo(_.content.debugConfiguration)),
    new BusyIndicatorHandler(zoomTo(_.content.showBusyIndicator)),
    new MenuHandler(zoomTo(_.content.menuModel)),
    new RuleResultsHandler(zoomTo(_.content.ruleResults)),
    new EOCacheHandler(zoomTo(_.content.cache)),
    new EOModelHandler(zoomTo(_.content.eomodel)),
    new PreviousPageHandler(zoomTo(_.content.previousPage))
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

    case InitClient =>
      value match {
        case Ready(eomodel) =>
          log.debug("FetchEOModel no change")
          noChange
        case _ =>
          log.debug("FetchEOModel fetching")
          effectOnly(Effect(AjaxClient[Api].fetchEOModel().call().map(SetEOModelThenFetchMenu(_))))
      }


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

    case SetEOModelThenFetchMenu(eomodel) =>
      updated(Ready(eomodel), Effect.action(FetchMenu))


    case SetEOModel(eomodel) =>
      updated(Ready(eomodel))


    // get the eomodel
    case NewEOWithEntityName(d2wContext, actions) =>
      log.debug("NewEOWithEntityName: " + d2wContext)
      // Create the EO and set it in the cache
      effectOnly(
        Effect.action(NewEOWithEOModel(value.get, d2wContext, actions)) // from edit ?
      )

    /*case NewEOWithEntityNameForEdit(selectedEntityName) =>
      log.debug("EOModelHandler | NewEOWithEntityNameForEdit " + selectedEntityName)
      effectOnly(
        Effect.action(NewEOWithEOModelForEdit(value.get, selectedEntityName)) // from edit ?
      )*/


  }

}

class DebugConfigurationHandler[M](modelRW: ModelRW[M, DebugConfiguration]) extends ActionHandler(modelRW) {
  override def handle = {
    case FetchShowD2WDebugButton =>
      log.debug("DebugHandler | FetchShowD2WDebugButton")
      effectOnly(Effect(AjaxClient[Api].getDebugConfiguration().call().map(SetDebugConfiguration(_))))

    case SetDebugConfiguration(debugConf) =>
      log.debug("DebugHandler | SetShowD2WDebugButton " + debugConf.showD2WDebugButton)
      updated(value.copy(showD2WDebugButton = debugConf.showD2WDebugButton))

    case SwithDebugMode =>
      log.debug("DebugHandler | SwithDebugMode")
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
      updated(true,Effect.action(Search(entityName)))
  }
}


class RuleResultsHandler[M](modelRW: ModelRW[M, Map[String,Map[String,Map[String,PageConfigurationRuleResults]]]]) extends ActionHandler(modelRW) {


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

  def updatedRuleResultsWithEntityMetaData(d2wContext: D2WContext, entityMetaData: EntityMetaData) = {
    // convert data from entityMetaData to ruleResults
    val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)

    log.debug("Register displayNameForEntity " + entityMetaData.displayName + " for task " + d2wContext.task)
    var updatedRuleResults = RuleUtils.registerRuleResult(value, RuleResult(fullFledged,RuleKeys.displayNameForEntity,RuleValue(stringV = Some(entityMetaData.displayName))))
    val displayPropertyKeys = entityMetaData.displayPropertyKeys map (p => p.name)
    updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, RuleResult(fullFledged,RuleKeys.displayPropertyKeys,RuleValue(stringsV = displayPropertyKeys)))

    for (prop <- entityMetaData.displayPropertyKeys) {
      val propertyD2WContext = fullFledged.copy(propertyKey = Some(prop.name))
      updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults,RuleResult(propertyD2WContext,RuleKeys.propertyType,RuleValue(stringV = Some(prop.typeV))))
      for (ruleResult <- prop.ruleResults) {
        updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults,ruleResult)
      }
    }
    updatedRuleResults
  }
  // handle actions
  override def handle = {

    /*case SetPageForTaskAndEntity(d2wContext) =>
      log.debug("SetPageForTaskAndEntity, d2wContext " + d2wContext)
      val entityName = d2wContext.entityName.get
      val metaDataPresent = RuleUtils.metaDataFetched(value,d2wContext)
      if (metaDataPresent) {
        log.debug("SetPageForTaskAndEntity, set page for entityName " + entityName)
        effectOnly(Effect.action(RegisterPreviousPage(d2wContext)))

      } else {
        log.debug("SetPageForTaskAndEntity, getMetaData for entityName " + entityName)
        val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
        effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaDataForMenu(d2wContext, _))))

      }

    case SetMetaDataForMenu(d2wContext, entityMetaData) => {
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults,Effect.action(RegisterPreviousPage(d2wContext)))
    }*/


    case SetMetaDataWithActions(d2wContext, actions, entityMetaData) =>
      log.debug("RuleResultsHandler | SetMetaDataWithActions " + entityMetaData + " actions: " + actions)

      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults,Effect.action(FireActions(d2wContext, actions)))

    case SetMetaData(d2wContext, entityMetaData) =>
      log.debug("SetMetaData " + entityMetaData)
      val updatedRuleResults = updatedRuleResultsWithEntityMetaData(d2wContext, entityMetaData)
      updated(updatedRuleResults)


    case FireActions(d2wContext: D2WContext, actions: List[D2WAction]) =>
      if (actions.isEmpty) {
        noChange
      } else {
        log.debug("RuleResultsHandler | FireActions " + actions.size + " actions: " + actions)


      // take first actions and call FireActions again with the rest
      val fireAction = actions.head
      val remainingActions = actions.tail
      fireAction match {
        case FireRule(rhs, key) =>


          log.debug("RuleResultsHandler | FireActions | Fire Rule " + key + " context: " + rhs)
          // convert any rhs depending on previous results

          //val newRhs = D2WContextUtils.convertD2WContextToFullFledgedByResolvingRuleFaults(value, rhs)
          val resolvedD2WContext = D2WContextUtils.d2wContextByResolvingRuleFaults(value,rhs)
          val alreadyExistRuleResult = RuleUtils.ruleResultForContextAndKey(value,resolvedD2WContext,key)
          alreadyExistRuleResult match {
            case Some(ruleResult) =>
              effectOnly(Effect.action(FireActions(d2wContext,remainingActions)))
            case None =>
              val newRhs = D2WContextUtils.convertD2WContextToFullFledged(resolvedD2WContext)

              log.debug("RuleResultsHandler | FireActions | Fire Rule | convertD2WContextToFullFledged " + newRhs)


              effectOnly(Effect(AjaxClient[Api].fireRule(newRhs,key).call().map(rr =>
              {
                //log.debug("rr " + rr)
                SetRuleResults(List(rr), d2wContext, remainingActions)
              })))
          }



        case CreateMemID(entityName) =>
          log.debug("RuleResultsHandler | FireActions | CreateMemID: " + entityName)
          effectOnly(Effect.action(NewEOWithEntityName(d2wContext,remainingActions)))

        case FetchMetaData(d2wContext) =>
          log.debug("RuleResultsHandler | FireActions | FetchMetaData: ")
          //val taskFault: TaskFault = rulesCon match {case taskFault: TaskFault => taskFault}
          val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
          effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaDataWithActions(d2wContext, remainingActions, _))))

        case FireRules(keysSubstrate, rhs, key) =>
          // TODO avoid double fetch


          log.debug("RuleResultsHandler | FireActions | FireRules: " + keysSubstrate)
          // For the moment, a KeySubstrate can have only a RuleFault
          // Let's resolve the fault (= firing the fault)
          val ruleResultOpt = RuleUtils.fireRuleFault(value, keysSubstrate.ruleFault.get)
          val updatedActions = ruleResultOpt match {
            case Some(RuleResult(rhs, rk, value)) => {
              //val ruleValue = rulesContainer.ruleResults
              val keys: Set[String] = rk match {
                case RuleKeys.keyWhenRelationship =>
                  Set(value.stringV.get)
                //Set(value)
                case RuleKeys.displayPropertyKeys =>
                  value.stringsV.toSet
                //Set(value)
              }
              val dc = D2WContextUtils.convertFullFledgedToD2WContext(rhs)
              val rulesActions = keys.toList.map(k => {
                val d2wContextProperty = dc.copy(propertyKey = Some(k))
                FireRule(d2wContextProperty,key)
              })
              remainingActions ::: rulesActions
            }
            case _ => remainingActions
          }
          effectOnly(Effect.action(FireActions(d2wContext, updatedActions)))


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
          log.debug("RuleResultsHandler | FireActions | Hydration: " + drySubstrate + " wateringScope: " + wateringScope)
          // We handle only RuleFault
          // -> we expect it

          // get displayPropertyKeys from previous rule results

          // How to proceed:
          // Using the d2wContext and the key to fire. We look inside the existing rules to get the rule result
          val ruleResultOpt = RuleUtils.fireRuleFault(value, wateringScope.fireRule.get)
          log.debug("Hydration with scope defined by rule " + ruleResultOpt)
          log.debug("Hydration drySubstrate " + drySubstrate)

          ruleResultOpt match {
            case Some(RuleResult(rhs,key,value)) => {

              //val ruleValue = rulesContainer.ruleResults
              val missingKeys: Set[String] = key match {
                case RuleKeys.keyWhenRelationship =>
                  Set(value.stringV.get)
                  //Set(value)
                case RuleKeys.displayPropertyKeys =>
                  value.stringsV.toSet
                  //Set(value)
              }
              log.debug("Hydration missingKeys " + missingKeys)

              drySubstrate match {
                case DrySubstrate(_ , Some(eoFault), _) =>
                  // completeEO ends up with a MegaContent eo update
                  effectOnly(Effect(AjaxClient[Api].completeEO(eoFault,missingKeys).call().map(UpdateRefreshEOInCache(_, d2wContext, remainingActions))))
                case DrySubstrate(Some(eoakp), _, _) =>
                  log.debug("Hydration | DrySubstrate " + eoakp.eo + " for key " + eoakp.keyPath)

                  val eovalueOpt = EOValue.valueForKey(eoakp.eo, eoakp.keyPath)
                  log.debug("Hydration | DrySubstrate eovalueOpt " + eovalueOpt)
                  eovalueOpt match {
                    case Some(eovalue) =>
                      eovalue match {
                        case ObjectsValue(pks) =>
                          log.debug("NVListComponent render pks " + pks)
                          effectOnly(Effect(AjaxClient[Api].hydrateEOs(rhs.entityName.get, pks, missingKeys).call().map(FetchedObjectsForEntity(_, d2wContext, remainingActions))))
                        case _ => effectOnly(Effect.action(FireActions(d2wContext, remainingActions))) // we skip the action ....
                      }
                    case _ => effectOnly(Effect.action(FireActions(d2wContext, remainingActions))) // we skip the action ....
                  }
                case DrySubstrate(_ , _ , Some(fs)) =>
                  log.debug("Hydration with fs " + fs)
                  fs match {
                    case fa: EOFetchAll => effectOnly(Effect(AjaxClient[Api].searchAll(fa).call().map(FetchedObjectsForEntity(_ , d2wContext, remainingActions))))
                    case fq: EOQualifiedFetch => effectOnly(Effect(AjaxClient[Api].search(fq).call().map(FetchedObjectsForEntity(_ , d2wContext, remainingActions))))
                  }


                case _ =>  effectOnly(Effect.action(FireActions(d2wContext,remainingActions))) // we skip the action ....
              }
            }
            case _ =>  effectOnly(Effect.action(FireActions(d2wContext,remainingActions))) // we skip the action ....
          }
      }
    }



      //case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
      //                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
      //case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer

    // many rules
    case SetRuleResults(ruleResults, d2wContext, actions: List[D2WAction]) =>
      log.debug("RuleResultsHandler | SetRuleResults " + ruleResults + " in d2w context " + d2wContext)
      log.debug("RuleResultsHandler | SetRuleResults | actions: " + actions)
      var updatedRuleResults = value
      for (ruleResult <- ruleResults) {
        updatedRuleResults = RuleUtils.registerRuleResult(updatedRuleResults, ruleResult)
      }
      updated(updatedRuleResults,Effect.action(FireActions(d2wContext, actions)))

  }
}


// hydrated destination EOs are simply stored in MegaContent eos
class EOCacheHandler[M](modelRW: ModelRW[M, EOCache]) extends ActionHandler(modelRW) {

  // eo chache is stored as:
  // Map of entityName -> Map of id -> eo
  // eos: Map[String, Map[Int,EO]],

  // Entity Name is retreived from the eos
  def updatedModelForEntityNamed(idExtractor: EO => Int, cache: Map[String, Map[Int, EO]], eos: Seq[EO]): Map[String, Map[Int, EO]] = {
    if (eos.isEmpty) {
      cache
    } else {

      val result: Map[String, Map[Int, EO]] = newUpdatedCache(idExtractor, cache, eos)
      log.debug("storing result as :" + result)
      result
    }
  }

  //EOValueUtils.pk(eo)
  def newUpdatedCache(idExtractor: EO => Int, cache: Map[String, Map[Int, EO]], eos: Seq[EO]): Map[String, Map[Int,EO]] = {
    val anyHead = eos.headOption
    anyHead match {
      case Some(head) =>
        val entity = head.entity
        val pkAttributeName = entity.pkAttributeName
        val entityName = entity.name
        val entityMap = if (cache.contains(entityName)) cache(entity.name) else Map.empty[Int, EO]

        // we create a Map with eo and id
        val refreshedEOs = eos.map(eo => {
          val pk = idExtractor(eo)
          Some((pk, eo))
        }).flatten.toMap

        // work with new and eo to be updated
        val refreshedPks = refreshedEOs.keySet
        val existingPks = entityMap.keySet

        val newPks = refreshedPks -- existingPks

        // Iterate on eos
        val newAndUpdateMap = refreshedPks.map(id => {
          log.debug("Refresh " + entityName + "[" + id +"]")
          val refreshedEO = refreshedEOs(id)
          log.debug("Refreshed " + refreshedEO)

          // 2 cases:
          // new
          if (newPks.contains(id)) {
            (id, refreshedEO)
          } else {
            // existing -> to be updated
            // Complete EOs !
            val existingEO = entityMap(id)
            (id, EOValue.completeEoWithEo(existingEO, refreshedEO))
          }
        }).toMap
        val newEntityMap = entityMap ++ newAndUpdateMap

        cache + (entity.name -> newEntityMap)
      case _ => Map.empty[String, Map[Int, EO]]
    }
  }

  def updatedOutOfDBCacheWithEOs(eos: Seq[EO]): EOCache = {
    log.debug("Cache before update " + value)
    val insertedEOs = value.insertedEOs
    val outOfDBEOs = updatedModelForEntityNamed(eo => eo.pk, value.eos, eos)
    val newCache = EOCache(outOfDBEOs, insertedEOs)
    log.debug("New cache " + newCache)
    newCache
  }


  def updatedMemCacheWithEOs(eos: Seq[EO]): EOCache = {
    val newCache = updatedModelForEntityNamed(eo => eo.pk, value.insertedEOs, eos)
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
    addEOToCache(eo, e => e.pk, eos)
  }
  def addEOToMemCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    addEOToCache(eo, e => -e.pk, eos)
  }

  def addEOToCache(eo: EO, idExtractor: EO => Int, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    updatedModelForEntityNamed(idExtractor, eos, Seq(eo))
  }

  def removeEOFromDBCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    removeEOFromCache(eo, e => e.pk, eos)
  }

  def removeEOFromMemCache(eo: EO, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    removeEOFromCache(eo, e => e.pk, eos)
  }

  def removeEOFromCache(eo: EO, idExtractor: EO => Int, eos: Map[String, Map[Int, EO]]): Map[String, Map[Int, EO]] = {
    val entityName = eo.entity.name
    val id = idExtractor(eo)
    log.debug("CacheHandler | removeEOFromCache " + id)

    val entityCache = eos(entityName)
    log.debug("CacheHandler | entityCache " + entityCache)

    val updatedEntityCache = entityCache - id
    log.debug("CacheHandler | updatedEntityCache " + updatedEntityCache)
    val updatedCache = eos + (entityName -> updatedEntityCache)
    log.debug("CacheHandler | updatedCache " + updatedCache)

    updatedCache
  }


  override def handle = {

    // EO goes from inserted EO to db eos
    case SavedEO(fromTask, eo) =>
      log.debug("CacheHandler | SavedEO " + eo)

      val isNewEO = EOValue.isNew(eo)
      // Adjust the insertedEOs cache
      val insertedEOs = if (isNewEO) removeEOFromMemCache(eo, value.insertedEOs) else value.insertedEOs
      log.debug("CacheHandler | SavedEO | removed if new  " + isNewEO)
      val updatedEO = if (isNewEO) {
        val pk = EOValue.pk(eo)
        eo.copy(pk = pk.get)
      } else eo
      log.debug("CacheHandler | SavedEO | updated insertedEOs  " + insertedEOs)
      log.debug("CacheHandler | SavedEO | register eo  " + updatedEO)

      // Adjust the db cache
      val eos = addEOToDBCache(updatedEO, value.eos)
      log.debug("CacheHandler | SavedEO | updated dbEOS  " + eos)

      val d2WContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.inspect), eo = Some(updatedEO))
      log.debug("CacheHandler | SavedEO update cache, call action Register with context " + d2WContext)
      updated(
        EOCache(eos, insertedEOs),
        Effect.action(RegisterPreviousPage(d2WContext))
      )

    case Save(selectedEntityName, eo) =>
      log.debug("CacheHandler | SAVE " + eo)
      // Update the DB and dispatch the result withing UpdatedEO action
      effectOnly(Effect(AjaxClient[Api].updateEO(eo).call().map(savingEO => {
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
         log.debug("CacheHandler | SaveNewEO " + eo)
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
      log.debug("CacheHandler | UpdateEOInCache " + eo)
      updated(
        updatedOutOfDBCacheWithEOs(Seq(eo))
      )

    case UpdateRefreshEOInCache(eo, property, actions) =>
      log.debug("CacheHandler | Refreshed EO " + eo)
      updated(
        updatedOutOfDBCacheWithEOs(Seq(eo)),
        Effect.action(FireActions(property,actions))
      )



    case RefreshedEOs(eoses) =>
      updated(
        updatedOutOfDBCacheWithEOs(eoses)
      )

    case FetchedObjectsForEntity(eoses, d2wContext, actions) =>
      log.debug("CacheHandler | FetchedObjectsForEntity d2wContext " + d2wContext)
      log.debug("CacheHandler | FetchedObjectsForEntity eoses " + eoses)

      updated(
        updatedOutOfDBCacheWithEOs(eoses),
        Effect.action(FireActions(d2wContext, actions))
      )

    case SearchResult(entityName, eoses) =>
      log.debug("CacheHandler | SearchResult length " + eoses.length)
      updated(
        updatedOutOfDBCacheWithEOs(eoses),
        Effect(AfterEffectRouter.setListPageForEntity(entityName))
      )
    case DeleteEOFromList(fromTask, eo) =>
      log.debug("CacheHandler | DeleteEOFromList " + eo)

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
      log.debug("CacheHandler | Deleted EO " + deletedEO)
      val entityName = deletedEO.entity.name
      val eoPk = EOValue.pk(deletedEO).get

      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap - eoPk
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))


    // set error on eo
    case UpdateEOsForEOOnError(eoOnError) =>
      log.debug("CacheHandler | UpdateEOsForEOOnError " + eoOnError)
      val escapedHtml = Utils.escapeHtml(eoOnError.validationError.get)
      val eoWithDisplayableError = eoOnError.copy(validationError = Some(escapedHtml))
      val entityName = eoOnError.entity.name
      val eoPk = EOValue.pk(eoWithDisplayableError).get
      val entityMap = value.eos(entityName)
      val newEntityMap = entityMap + (eoPk -> eoWithDisplayableError)
      val newValue = value.eos + (entityName -> newEntityMap)
      updated(updatedOutOfDBCache(newValue))


    case UpdateEOValueForProperty(eo, d2wContext, newEOValue) =>
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get

      log.debug("CacheHandler | Update EO Property: for entity " + entityName + " property: " + propertyName + " " + newEOValue)
      //val modelWriter: ModelRW[M, EO] = AppCircuit.zoomTo(_.get)
      //val propertyValueWriter = zoomToPropertyValue(property,modelRW)
      // case class EO(entity: String, values: scala.collection.Map[String,EOValue])
      log.debug("EO: " + eo)
      val updatedEO = eo.copy(values = (eo.values - propertyName) + (propertyName -> newEOValue))

      if (updatedEO.pk < 0) {
        updated(updatedMemCacheWithEOs(Seq(updatedEO)))
      } else {
        updated(updatedOutOfDBCacheWithEOs(Seq(updatedEO)))
      }

    case NewEOWithEOModel(eomodel, d2wContext, actions: List[D2WAction]) =>
      log.debug("CacheHandler | NewEOWithEOModel: " + d2wContext)
      val entityName = d2wContext.entityName.get
      // Create the EO and set it in the cache
      val (newValue, newEO) = EOValue.createAndInsertNewObject(value.insertedEOs, eomodel, entityName)

      log.debug("newValue " + newValue)
      log.debug("newEO " + newEO)

      //val eo = EOValue.memEOWith(eomodel, entityName, newEO.pk)
      val newD2WContext = d2wContext.copy(eo = Some(newEO))
      updated(updatedMemCache(newValue),
              Effect.action(FireActions(newD2WContext,actions)))

    case NewEOWithEOModelForEdit(entity) =>
      val entityName = entity.name
      log.debug("CacheHandler | NewEOWithEOModelForEdit " + entityName)
      val (newValue, newEO) = EOValue.createAndInsertNewObject(value.insertedEOs, entity)

      log.debug("newValue " + newValue)
      log.debug("newEO " + newEO)
      //val eo = EOValue.memEOWith(eomodel, entityName, newEO.memID)

      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.edit), eo = Some(newEO))

      updated(updatedMemCache(newValue),
        Effect.action(RegisterPreviousPage(d2wContext)))


  }

}

class PreviousPageHandler[M](modelRW: ModelRW[M, Option[D2WContext]]) extends ActionHandler(modelRW) {

  def stackD2WContext(d2WContext: D2WContext) : D2WContext = {
    value match {
      case Some(currentPage) => {
        d2WContext.copy(previousTask = value)
      }
      case _ => d2WContext
    }
  }

  override def handle = {

    case  InspectEO (fromTask, eo) =>
      log.debug("PreviousPageHandler | InspectEO from: " + fromTask)
      val d2wContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.inspect), eo = Some(eo))
      effectOnly(Effect.action(RegisterPreviousPage(d2wContext)))


    case  InitMetaDataForList (entityName) =>
      log.debug("PreviousPageHandler | InitMetaDataForList for: " + entityName)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list))
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)

      effectOnly(Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaData(d2wContext,_))))

    case InitMetaData(entityName) =>
      log.debug("PreviousPageHandler | InitMetaData for Query page: " + entityName)
      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.query))
      val fullFledged = D2WContextUtils.convertD2WContextToFullFledged(d2wContext)
      updated(
        // change context to inspect
        Some(d2wContext),
        Effect(AjaxClient[Api].getMetaData(fullFledged).call().map(SetMetaData(d2wContext,_))))

    case RegisterPreviousPage(d2WContext) =>
      val  stack = stackD2WContext(d2WContext)
      log.debug("PreviousPageHandler | RegisterPreviousPage for d2wContext: " + stack)
      updated(
            // change context to inspect
            Some(stack),
            Effect(AfterEffectRouter.setPageForTaskAndEOAndEntity(d2WContext))
          )

    case SetPreviousPage =>
      log.debug("PreviousPageHandler | SetPreviousPage to: " + value)
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
      log.debug("PreviousPageHandler | UpdateQueryProperty: for entity " + entityName + " with queryValue " + queryValue)
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues + (queryValue.key -> queryValue)
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case ClearQueryProperty(entityName, propertyName) =>
      log.debug("PreviousPageHandler | ClearQueryProperty: for entity " + entityName + " and property " + propertyName)
      val d2wContext = value.get
      val currentQueryValues = d2wContext.queryValues
      val newQueryValues = currentQueryValues - propertyName
      val newD2wContext = d2wContext.copy(queryValues = newQueryValues)
      updated(Some(newD2wContext))

    case Search(entityName) =>
      log.debug("PreviousPageHandler | Search | " + entityName + " | value: " + value)
      val fs: EOFetchSpecification = value match {
        case Some(d2wContext) =>
          val qualifierOpt = QueryValue.qualifierFromQueryValues(d2wContext.queryValues.values.toList)
          log.debug("SPACircuit | Search(" + entityName + ") | qualifierOpt: " + qualifierOpt)

          qualifierOpt match {
              // TODO manage sort ordering
            case Some(qualifier) => EOQualifiedFetch(entityName,qualifier,List())
            case _ =>   EOFetchAll(entityName)
          }
        case _ => EOFetchAll(entityName)  // shouldn't come here because the query page initialized it
      }
      log.debug("PreviousPageHandler | Search | " + entityName + " query with fs " + fs)
      // Call the server to get the result +  then execute action Search Result (see above datahandler)

      val d2wContext = D2WContext(entityName = Some(entityName), task = Some(TaskDefine.list), dataRep = Some(DataRep(Some(fs))))
      val  stack = stackD2WContext(d2wContext)
      log.debug("PreviousPageHandler | Search | Register Previous " + stack)

      val effect = fs match {
        case fa: EOFetchAll => Effect(AjaxClient[Api].searchAll(fa).call().map(SearchResult(entityName, _)))
        case fq: EOQualifiedFetch => Effect(AjaxClient[Api].search(fq).call().map(SearchResult(entityName, _)))
      }

      updated(
        // change context to inspect
        Some(stack),
        effect
      )

  }
}


class MenuHandler[M](modelRW: ModelRW[M, Pot[Menus]]) extends ActionHandler(modelRW) {

  override def handle = {
    case FetchMenu =>
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


  }

}

