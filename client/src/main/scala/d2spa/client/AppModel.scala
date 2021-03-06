package d2spa.client

import diode._
import diode.data.{Pot, _}
import diode.util._
import d2spa.shared.{D2WContext, EOValue, EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.shared.WebSocketMessages.RuleToFire
import jdk.nashorn.internal.ir.PropertyKey
import diode.Action
import upickle.default.{macroRW, ReadWriter => RW}
/**
  * Created by dschoen on 01.05.17.
  */



sealed trait RulesContainer {
  def ruleResults: List[RuleResult]
}

case class AppConfiguration(serverAppConf: DebugConf = DebugConf(true), isDebugMode: Boolean = false, fetchMenus: Boolean = true, socketReady: Boolean = false)

case class PageConfigurationRuleResults(override val ruleResults: List[RuleResult] = List(), properties: Map[String,PropertyRuleResults] = Map()) extends RulesContainer
case class PropertyRuleResults(override val ruleResults: List[RuleResult] = List(), typeV: String = "stringV") extends RulesContainer
object PageConfiguration {
  val NoPageConfiguration = "NoPageConfiguration"
}

case class D2WContextEO(pk: Option[Int] = None, memID : Option[Int] = None)


// cache could be filled with an FS which could fill it partially
// how to know if it is filled for the FS we want ?
// store qualifier -> eos
// if if contains an entry for the qualifier or if partial a full exists

// data structure
// Map: entity --> EntityCache( full : Some(Map[EOPk,EO]]), partials : Map[EOQualifier, Map[EOPk,EO]])
// or
// Map: entity --> Map[EOQualifier, Map[EOPk,EO]])
// if only 1 qualifier and is EmptyQualifier
// EmptyQualifier has to be used also for search in that way we can merge fetch and fetch all

// server knows also qualifier --> eos
// if new eo or eo updated -> if validate for qualifier -> send update of qualifier --> eos

// mechanism for qualifier inclusion i.e if FetchAll not need to store something else
// not storing a partial if an all (at client and server) Note: if a partial is executed on the client and an full exists, it will not go to the server

// the server could decide to:
// return an all for a partial if not so many rows
// return a batch of a partial or full if too many rows

// concept of qualifier / Batch number --> eos

sealed trait EOCacheEntityElement
case class EODBCacheEntityElement(data : Pot[Map[EOPk,EOContaining]], hasAllRecords: Boolean = false) extends EOCacheEntityElement
case class EOMemCacheEntityElement(data : Map[EOPk,EOContaining]) extends EOCacheEntityElement


case class EOCache(eomodel: Pot[EOModel],
                   eos: Map[String, EOCacheEntityElement],
                   insertedEOs: Map[String,EOCacheEntityElement])


case class SearchInMemory(fs: EOFetchSpecification) extends Action
case class DebugConf(showD2WDebugButton: Boolean)
case class SendingAction(action: Action) extends Action

// define actions
case object ShowBusyIndicator extends Action
case object HideBusyIndicator extends Action
case class SearchWithBusyIndicator(entityName: String) extends Action

case object SocketReady extends Action
case class InitAppSpecificClient(d2wContext: PageContext) extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus, d2wContext: D2WContext) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
//case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
//case class UpdateRefreshEOInCache(eos: Seq[EO], d2wContext: PageContext, actions: List[D2WAction]) extends Action
case class UpdateEOInCache(eo:EO) extends Action
case object FetchEOModel extends Action
case class FetchEOModelAndMenus(d2wContext: D2WContext) extends Action


case class SetEOModelThenFetchMenu(eomodel: EOModel, d2wContext: D2WContext) extends Action
case class SetEOModel(eomodel: EOModel) extends Action
case class FetchMenu(d2wContext: D2WContext) extends Action

case class FetchedObjectsForEntity(entityName: String, eos: Seq[EOContaining], ruleResults: Option[List[RuleResult]]) extends Action
case class CompletedEO(d2wContext: Option[D2WContext], hydration: Hydration, eo: List[EOContaining], ruleResults: Option[List[RuleResult]]) extends Action
case class CompletedEOProcessing(d2wContext: Option[D2WContext], hydration: Option[Hydration], eo: List[EOContaining], ruleResults: Option[List[RuleResult]]) extends Action
case class SetPageForSocketReady(d2wContext: PageContext) extends Action
case class RegisterEO(eo: EOContaining) extends Action
case class RefreshedEOs(eos: Seq[EO])

case class SendRuleRequest(ruleRequest: RuleRequest) extends Action
//case class SetPageForTaskAndEntity(d2wContext: D2WContext) extends Action
case class CreateEO(entityName:String) extends Action

case class SetMetaData(d2wContext: D2WContext, ruleResults: Option[List[RuleResult]]) extends Action
case class SetRulesForPrepareEO(d2wContext: D2WContext, ruleResults: Option[List[RuleResult]], eoOpt: Option[EOContaining]) extends Action
//case class SetMetaDataForMenu(d2wContext: D2WContext, metaData: EntityMetaData) extends Action

case class NewAndRegisteredEO(d2wContext: PageContext) extends Action
case class RegisteredExistingEO(d2wContext: PageContext) extends Action


//case class NewEOCreated(eo: EO, d2wContext: PageContext, actions: List[D2WAction]) extends Action
case class NewEOCreatedForEdit(eo: EO) extends Action

case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case object SetPreviousPage extends Action

case class RegisterAndPrepareEODisplay(eo: EOContaining, d2wContext: PageContext) extends Action
case class PrepareEODisplay(d2wContext: PageContext) extends Action
case class PrepareEditPage(d2wContext: PageContext) extends Action
case class SearchHydration(fs: EOFetchSpecification) extends Action


case class PrepareEODisplayRules(d2wContext: PageContext, cache: EOCache, needsHydration: Boolean) extends Action
case class HydrationRequest(hydration: Hydration, ruleRequest: Option[RuleRequest]) extends  Action

case class RegisterPreviousPage(d2wContext: PageContext) extends Action
case class RegisterPreviousPageAndSetPage(d2wContext: PageContext) extends Action
case class RegisterPreviousPageAndSetPagePure(d2wContext: PageContext) extends Action
case class RegisterPreviousPageAndSetPageRemoveMemEO(d2wContext: PageContext, eo: EOContaining) extends Action

case class SetPage(d2WContext: PageContext) extends Action
case class UpdateCurrentContextWithEO(eo: EOContaining) extends Action
case object InitMenuSelection extends Action

case object InitAppModel extends Action
case class ShowFetchResults(fs: EOFetchSpecification) extends Action

case class SelectMenu(entityName: String) extends Action
case class Save(entityName: String, eo: EOContaining) extends Action
case class GoSaving(entityName: String, eo: EOContaining) extends Action

case class SavingEO(d2wContext: D2WContext, eo: EOContaining, ruleResults: Option[List[RuleResult]]) extends Action
case class SaveNewEO(entityName: String, eo: EOContaining) extends Action
case class PrepareSearchForServer(d2wContext: PageContext, ruleRequest: RuleRequest) extends Action

case class UpdateQueryProperty(entityName: String, queryValue: QueryValue) extends Action
case class ClearQueryProperty(entityName: String, propertyName: String, operator: String) extends Action
case class UpdateEOValueForProperty(eo: EOContaining, d2wContext: PageContext, value: EOValue) extends Action

case class SearchAction(entityName: String) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(fs: EOFetchSpecification, eos: Seq[EOContaining]) extends Action
case class SearchResultWithRuleResults(fs: EOFetchSpecification, eos: Seq[EOContaining], ruleResults: Option[List[RuleResult]]) extends Action
case class RegisterSearchResults(fs: EOFetchSpecification, eos: Seq[EOContaining]) extends Action
case class CacheForPrepareEODisplay(eo: EOContaining, pageContext: PageContext) extends Action


// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class SavedEO(dw2Context: PageContext) extends Action
case class SavedEOWithResults(fromTask: String, eo: EOContaining, dw2Context: PageContext, ruleResults: Option[List[RuleResult]]) extends Action

case class DeleteEO(fromTask: String, eo: EOContaining) extends Action
case class DeleteEOFromList(eo: EOContaining) extends Action
case class DeletingEO(eo: EOContaining) extends Action
case class EditEO(dw2Context: PageContext) extends Action
case class EditEOWithResults(fromTask: String, eo: EOContaining, dw2Context: PageContext, ruleResults: Option[List[RuleResult]]) extends Action
case class InspectEO(fromTask: String, eo: EOContaining) extends Action

case class SaveError(eo: EOContaining) extends Action
object ListEOs extends Action
case class DeletedEO(eo:EOContaining) extends Action
case class UpdateEOsForEOOnError(eo: EOContaining) extends Action
case class ShowResults(fs: EOFetchSpecification) extends Action

//trait D2WAction extends diode.Action

// Typical case:
// key : componentName
// keysSubstrate: displayPropertyKeys either defined by a rule result or a rule to fire
// In that case the FireRules will trigger RuleToFire action to the server for as many entries in dipslayPropertyKeys
// case class CreateMemID(entityName: String) extends D2WAction
case class GetMetaDataForSetPage(d2wContext: PageContext) extends Action


object HydrationUtils {

  def toFault(eoContaining : EOContaining) = EOFault(eoContaining.eo.entityName, eoContaining.eo.pk)

  // case class DrySubstrate(eosAtKeyPath: Option[EOsAtKeyPath] = None, eo: Option[EOFault] = None, fetchSpecification: Option[EOFetchSpecification] = None)

  def entityName(drySubstrate: DrySubstrate) = drySubstrate match {
    case DrySubstrate(Some(eosFault), _ , _) => eosFault.entityName
    case DrySubstrate(_, Some(eoFault) , _) => eoFault.entityName
    case DrySubstrate(_, _ , Some(fs)) => EOFetchSpecification.entityName(fs)
  }

  def isEOHydrated(cache: EOCache, entityName: String, pk : EOPk, propertyKeys: List[String]) : Boolean = {
    val eoContainingOpt = EOCacheUtils.outOfCacheEOUsingPk(cache, entityName, pk)
    eoContainingOpt match {
      case Some(eoContaining) =>
        val eo = eoContaining.eo
        val valuesKeys = eo.values
        val anyMissingPropertyKey = propertyKeys.find(p => !valuesKeys.contains(p))
        anyMissingPropertyKey.isEmpty
      case None => false
    }

  }

  def needsHydration(drySubstrateOpt: Option[DrySubstrate], displayPropertyKeysRuleResultPot: PotFiredRuleResult, cache: EOCache, eomodel: EOModel) :Boolean = {
    drySubstrateOpt match {
      case Some(drySubstrate) =>

        val displayPropertyKeysOpt = RuleUtils.ruleListValueFromPotFiredRuleResult(displayPropertyKeysRuleResultPot)
        displayPropertyKeysOpt match {
          case Some(displayPropertyKeys) =>
            val isHydrated = HydrationUtils.isHydratedForPropertyKeys(eomodel, cache, drySubstrate, displayPropertyKeys)
            !isHydrated
          case None =>
            true
        }

      case None =>
        false
    }


  }


  def drySubstrateFromDataRep(dataRep: Option[DataRep]) :Option[DrySubstrate]  = {
    dataRep match {
      case Some(DataRep(Some(fetchSpecification), _)) =>
        Some(DrySubstrate(fetchSpecification = Some(fetchSpecification)))

      case Some(DataRep(_, Some(eoakp))) => {
        val eovalueOpt = eoakp.eoContaining.valueForKey(eoakp.keyPath)
        eovalueOpt match {
          case Some(eovalue) =>
            eovalue match {
              case ObjectsValue(pks) =>
                val entityName = eoakp.destinationEntityName
                val eosFault =  EOsFault(entityName, pks)
                Some(DrySubstrate(eos = Some(eosFault)))
              case _ => None
            }
          case None => None
        }
      }
      case _ => None
    }
  }



  // EOs are supposed to exists in cache but not hydrated correctly in scope
  def isHydratedForPropertyKeys(eomodel: EOModel, cache: EOCache, drySubstrate: DrySubstrate, propertyKeys: List[String]): Boolean = {
    true
  }
  /* Materialooze
  def isHydratedForPropertyKeys(eomodel: EOModel, cache: EOCache, drySubstrate: DrySubstrate, propertyKeys: List[String]): Boolean = {
    drySubstrate match {
      case DrySubstrate(_, Some(eoFault), _) =>
        isEOHydrated(cache,eoFault.entityName, eoFault.pk, propertyKeys)
      case DrySubstrate(Some(eosFault), _, _) =>
        val destinationEntityName = eosFault.entityName
        val nonHydrated = eosFault.pks.find(pk => !isEOHydrated(cache, destinationEntityName, pk, propertyKeys))
        nonHydrated.isEmpty
      case DrySubstrate(_, _, Some(fs)) =>
        val eos = EOCacheUtils.objectsFromAllCachesWithFetchSpecification(cache,fs)
        val nonHydrated = eos.find(eo => !isEOHydrated(cache, eo.entityName, eo.pk, propertyKeys))
        !nonHydrated.isDefined
    }
  }*/

}

object QueryOperator {
  val Match = "Match"
  val Min = "Min"
  val Max = "Max"
}


// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String, value: EOValue, operator: String)





object QueryValue {
  val operatorByQueryOperator = Map(
    QueryOperator.Max -> NSSelector.QualifierOperatorLessThanOrEqualTo,
    QueryOperator.Min -> NSSelector.QualifierOperatorGreaterThanOrEqualTo,
    QueryOperator.Match -> NSSelector.QualifierOperatorEqual
  )

  def qualifierFromQueryValues(pageQueryValues: PageQueryValues) : Option[EOQualifier] = {
    val matchQualifiers = qualifiersWithQueryValues(pageQueryValues.queryMatch.values.toList)
    val minQualifiers = qualifiersWithQueryValues(pageQueryValues.queryMin.values.toList)
    val maxQualifiers = qualifiersWithQueryValues(pageQueryValues.queryMax.values.toList)
    val qualifiers = matchQualifiers ::: minQualifiers ::: maxQualifiers

    if (qualifiers.isEmpty) None else Some(EOAndQualifier(qualifiers))
  }
  def qualifiersWithQueryValues(queryValues: List[QueryValue]) = {
    queryValues.map(qv => {
      log.finest("QueryValue " + qv)
      log.finest("QueryValue operatorByQueryOperator(qv.operator): " + operatorByQueryOperator(qv.operator))
      EOKeyValueQualifier(qv.key,operatorByQueryOperator(qv.operator),qv.value)
    })
  }

  def size(queryValue: QueryValue) = EOValue.size(queryValue.value)
}


case class PageQueryValues(
                            queryMatch: Map[String, QueryValue] = Map(),
                            queryMin: Map[String, QueryValue] = Map(),
                            queryMax: Map[String, QueryValue] = Map()
                          )

//implicit val fireActionPickler = CompositePickler[FireAction].

// The D2WContext will contains also the queryValues
// it should contain everything needed to redisplay a page
case class PageContext(previousTask: Option[PageContext] = None,
                       queryValues: PageQueryValues = PageQueryValues(),
                       dataRep: Option[DataRep] = None,
                       eo: Option[EOContaining] = None,
                       d2wContext: D2WContext
                      )

// Right = Some
// here right is an result which could be Some or None
// left is not yet a result because it is a rule to be fired
case class PotFiredKey (value: Either[FireRule, Option[String]])

case class DataRep (fetchSpecification: Option[EOFetchSpecification] = None, eosAtKeyPath: Option[EOsAtKeyPath] = None)



case class KeysSubstrate(ruleResult: PotFiredRuleResult)


//case class EORefsDefinition()
case class EOsAtKeyPath(eoContaining: EOContaining, keyPath: String, destinationEntityName: String)
//case class RuleRawResponse(ruleFault: RuleFault, response: WSResponse)




object RuleUtils {
  def pageContextWithD2WContext(d2wContext: D2WContext) = {
    PageContext(d2wContext = d2wContext)
  }


  def metaDataRuleRequest(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]],d2wContext: D2WContext) = {
    val rules = RuleUtils.metaDataFiringRules(ruleResults, d2wContext)
    RuleRequest(d2wContext,rules)
  }


  def ruleRequestWithRules(d2wContext: D2WContext, rules: List[FiringRules]) : Option[RuleRequest] =
    if (rules.isEmpty) {
      None
    } else {
      Some(RuleRequest(d2wContext,rules))
    }

  def metaDataFiringRules(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]],d2wContext: D2WContext): List[FiringRules] = {
    val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
    val displayNameForEntityRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.displayNameForEntity)

    val componentNameFireRulesOpt = RuleUtils.potentialFireRules(ruleResults, d2wContext, displayPropertyKeysRuleResultPot, RuleKeys.componentName)
    val displayNameForPropertyFireRulesOpt = RuleUtils.potentialFireRules(ruleResults, d2wContext, displayPropertyKeysRuleResultPot, RuleKeys.displayNameForProperty)
    val attributeTypeFireRulesOpt = RuleUtils.potentialFireRules(ruleResults, d2wContext, displayPropertyKeysRuleResultPot, RuleKeys.attributeType)

    val fireSingleRulesPots = List(displayPropertyKeysRuleResultPot, displayNameForEntityRuleResultPot)
    val fireSingleRules = firingRulesFromPotFiredRuleResult(fireSingleRulesPots)

    val fireRulesOpts = List(componentNameFireRulesOpt, displayNameForPropertyFireRulesOpt, attributeTypeFireRulesOpt)
    val fireRules = firingRulesFromFireRules(fireRulesOpts)

    fireSingleRules ::: fireRules
  }

  def firingRulesFromFireRules(fireRulesOpts: List[Option[FiringRules]]): List[FiringRules] = fireRulesOpts.flatten


  def firingRulesFromPotFiredRuleResult(fireSingleRulesPots: List[PotFiredRuleResult]): List[FiringRules] = {
    val fireSingleRulesOpts = fireSingleRulesPots.map(pot => pot match {
      case PotFiredRuleResult(Left(fireRuleKey)) => Some(FireRule(fireRuleKey))
      case _ => None
    })
    fireSingleRulesOpts.flatten
  }

  def potentialFireRuleResultPot(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String): PotFiredRuleResult = {
    val ruleResultOpt: Option[RuleResult] = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleResultOpt match {
      case Some(ruleResult) => PotFiredRuleResult(Right(ruleResult))
      case None => PotFiredRuleResult(Left(key))
    }
  }

  def potentialFireRule(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String): Option[FireRule] = {
    val ruleResultOpt: Option[RuleResult] = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleResultOpt match {
      case Some(ruleResult) => None
      case None => Some(FireRule(key))
    }
  }


  def pageConfigurationRuleResultsForContext(
                                              ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]],
                                              d2wContext: D2WContext
                                            ): Option[PageConfigurationRuleResults] = {
    val entityName = d2wContext.entityName.get
    val task = d2wContext.task.get
    val pageConfiguration: String = d2wContext.pageConfiguration match {
      case Some(pc) => pc
      case None => PageConfiguration.NoPageConfiguration
    }
    if (ruleResults.contains(entityName)) {
      val entitySubTree = ruleResults(entityName)
      if (entitySubTree.contains(task)) {
        val taskSubTree = entitySubTree(task)
        if (taskSubTree.contains(pageConfiguration)) {
          val pageConfigurationSubTree = taskSubTree(pageConfiguration)
          Some(pageConfigurationSubTree)
        } else None
      } else None
    } else None
  }

  def ruleContainerForContext(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext): Option[RulesContainer] = {
    val pageConfigurationRuleResultsOpt = pageConfigurationRuleResultsForContext(ruleResults, d2wContext)
    pageConfigurationRuleResultsOpt match {
      case Some(pageConfigurationRuleResults) =>
        d2wContext.propertyKey match {
          case Some(propertyKey) => {
            if (pageConfigurationRuleResults.properties.contains(propertyKey)) {
              Some(pageConfigurationRuleResults.properties(propertyKey))
            } else {
              None
            }
          }
          case _ => {
            Some(pageConfigurationRuleResults)
          }
        }
      case _ => None
    }
  }


  def ruleResultForContextAndKey(
                                  ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]],
                                  d2wContext: D2WContext,
                                  key: String): Option[RuleResult] = {
    val ruleContainerOpt = ruleContainerForContext(ruleResults, d2wContext)
    ruleContainerOpt match {
      case Some(rulesContainer) => {
        RulesUtilities.ruleResultFromRuleResultsForContextAndKey(rulesContainer.ruleResults, d2wContext, key)
      }
      case _ => None
    }
  }


  def ruleStringValueFromPotFiredRuleResult(potFiredRuleResult: PotFiredRuleResult): Option[String] = {
    ruleResultValueFromPotFiredRuleResult(potFiredRuleResult) match {
      case Some(ruleResult) => ruleStringValueWithRuleResult(Some(ruleResult))
      case None =>
        None
    }
  }

  def ruleListValueFromPotFiredRuleResult(potFiredRuleResult: PotFiredRuleResult) = {
    ruleResultValueFromPotFiredRuleResult(potFiredRuleResult) match {
      case Some(ruleResult) => Some(RulesUtilities.ruleListValueWithRuleResult(Some(ruleResult)))
      case None => None
    }
  }
  def ruleResultValueFromPotFiredRuleResult(potFiredRuleResult: PotFiredRuleResult) = {
    potFiredRuleResult match {
      case PotFiredRuleResult(Right(ruleResult)) => Some(ruleResult)
      case _ => None
    }
  }



  def missingKeysForKey(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, propertyKeys: List[String], key: String) = {
    val missingPropertyKeys: List[String] = propertyKeys.filter(propertyKey => {
      val propertyD2WContext = d2wContext.copy(propertyKey = Some(propertyKey))
      val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, propertyD2WContext, key)
      ruleResultOpt.isEmpty
    })
    missingPropertyKeys
  }

  // in case the PotFiredRuleResult is non empty, we look for each property and return a list of only those missing
  def potentialFireRules(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]],
                         d2wContext: D2WContext,
                         propertyKeysRuleResultPot: PotFiredRuleResult,
                         key: String): Option[FiringRules] = {

    propertyKeysRuleResultPot match {
      case PotFiredRuleResult(Right(ruleResult)) =>
        val propertyKeys: List[String] = RulesUtilities.ruleListValueWithRuleResult(Some(ruleResult))
        val missingPropertyKeys = missingKeysForKey(ruleResults, d2wContext, propertyKeys, key)

        if (missingPropertyKeys.size == 0) {
          None
        } else {
          Some(FireRules(missingPropertyKeys, key))
        }
      case PotFiredRuleResult(Left(keyToFire)) =>
        Some(GappedFireRules(keyToFire, key))
      case _ => None
    }
  }





  //def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {r.key.equals(key)})

  def ruleStringValueForContextAndKey(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String): Option[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleStringValueWithRuleResult(ruleResult)
  }

  def ruleBooleanValueForContextAndKey(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String): Boolean = {
    val booleanAsStringOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, key)
    booleanAsStringOpt match {
      case Some(boolVal) => boolVal.equals("true")
      case None => false
    }
  }

  def ruleListValueForContextAndKey(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String): List[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    RulesUtilities.ruleListValueWithRuleResult(ruleResult)
  }



  def pageConfigurationWithRuleResult(propertyKeyOpt: Option[String], ruleResult: RuleResult) = {
    propertyKeyOpt match {
      case Some(propertyKey) => {
        PageConfigurationRuleResults(properties = Map(propertyKey -> PropertyRuleResults(ruleResults = List(ruleResult))))
      }
      case _ => {
        PageConfigurationRuleResults(ruleResults = List(ruleResult))
      }
    }
  }


  def registerRuleResult(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], ruleResult: RuleResult): Map[String, Map[String, Map[String, PageConfigurationRuleResults]]] = {
    val d2wContext = ruleResult.rhs
    val entityName = d2wContext.entityName.get
    val task = d2wContext.task.get
    val pageConfiguration: String = if (d2wContext.pageConfiguration.isDefined) d2wContext.pageConfiguration.get else PageConfiguration.NoPageConfiguration


    val taskSubTree = if (ruleResults.contains(entityName)) {
      val existingTaskSubTree = ruleResults(entityName)
      val pageConfigurationSubTree = if (existingTaskSubTree.contains(task)) {
        val existingPageConfigurationSubTree = existingTaskSubTree(task)
        val pageConfigurationRuleResults = if (existingPageConfigurationSubTree.contains(pageConfiguration)) {
          val existingPageConfigurationRuleResults = existingPageConfigurationSubTree(pageConfiguration)
          d2wContext.propertyKey match {
            case Some(propertyKey) => {
              val propertyRuleResults = if (existingPageConfigurationRuleResults.properties.contains(propertyKey)) {
                val existingPropertyRuleResults = existingPageConfigurationRuleResults.properties(propertyKey)
                existingPropertyRuleResults.copy(ruleResults = ruleResult :: existingPropertyRuleResults.ruleResults)
              } else {
                PropertyRuleResults(ruleResults = List(ruleResult))
              }
              val newProperties = existingPageConfigurationRuleResults.properties + (propertyKey -> propertyRuleResults)
              existingPageConfigurationRuleResults.copy(properties = newProperties)
            }
            case _ => {
              existingPageConfigurationRuleResults.copy(ruleResults = ruleResult :: existingPageConfigurationRuleResults.ruleResults)
            }
          }
        } else {
          pageConfigurationWithRuleResult(d2wContext.propertyKey, ruleResult)
        }
        existingPageConfigurationSubTree + (pageConfiguration -> pageConfigurationRuleResults)
      } else {
        val pageConfigurationRuleResults = pageConfigurationWithRuleResult(d2wContext.propertyKey, ruleResult)
        Map(pageConfiguration -> pageConfigurationRuleResults)
      }
      existingTaskSubTree + (task -> pageConfigurationSubTree)
    } else {
      val pageConfigurationRuleResults = pageConfigurationWithRuleResult(d2wContext.propertyKey, ruleResult)
      Map(task -> Map(pageConfiguration -> pageConfigurationRuleResults))
    }
    ruleResults + (entityName -> taskSubTree)
  }




  def ruleStringValueWithRuleResult(ruleResultOpt: Option[RuleResult]) = {
    ruleResultOpt match {
      case Some(ruleResult) => ruleResult.value.stringV
      case _ => None
    }
  }


  def existsRuleResultForContextAndKey(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String) =
    ruleResultForContextAndKey(ruleResults, d2wContext, key).isDefined


}

object D2WContextUtils {


  def pageContextUpdatedWithQueryValue(pageContext: PageContext, queryValue: QueryValue) = {
    val operator = queryValue.operator
    val queryMap = queryMapWithOperator(pageContext, operator)

    val newQueryMap = queryMap + (queryValue.key -> queryValue)
    val pageQueryValues = updatePageQueryValuesForOperator(pageContext.queryValues, newQueryMap, operator)
    pageContext.copy(queryValues = pageQueryValues)
  }

  def updatePageQueryValuesForOperator(pageQueryValues: PageQueryValues, queryMap: Map[String, QueryValue], operator: String): PageQueryValues = {
    operator match {
      case QueryOperator.Match =>
        pageQueryValues.copy(queryMatch = queryMap)
      case QueryOperator.Min =>
        pageQueryValues.copy(queryMin = queryMap)
      case QueryOperator.Max =>
        pageQueryValues.copy(queryMax = queryMap)
    }
  }

  def updatePageContextByClearingKeyForOperator(pageContext: PageContext, key: String, operator: String) = {
    val queryMap = queryMapWithOperator(pageContext, operator)
    val newQueryMap = queryMap - key
    val pageQueryValues = updatePageQueryValuesForOperator(pageContext.queryValues, newQueryMap, operator)
    pageContext.copy(queryValues = pageQueryValues)
  }

  def queryMapWithOperator(pageContext: PageContext, operator: String) = {
    operator match {
      case QueryOperator.Match =>
        pageContext.queryValues.queryMatch
      case QueryOperator.Min =>
        pageContext.queryValues.queryMin
      case QueryOperator.Max =>
        pageContext.queryValues.queryMax
    }
  }

  def queryValueForKey(pageContext: PageContext, key: String, operator: String) = {
    val queryMap = queryMapWithOperator(pageContext, operator)
    if (queryMap.contains(key)) Some(queryMap(key).value) else None
  }

  def queryValueAsStringForKey(pageContext: PageContext, key: String): String = {
    queryValueAsStringForKey(pageContext, key, QueryOperator.Match)
  }

  def queryValueAsStringForKey(d2wContext: PageContext, key: String, operator: String): String = {
    val value = D2WContextUtils.queryValueForKey(d2wContext, key, operator)
    value match {
      case Some(StringValue(str)) => str
      case Some(IntValue(str)) => str.toString
      case _ => ""
    }
  }





}


//case class SetRuleResults(ruleResults: List[RuleResult], d2wContext: PageContext, actions: List[D2WAction]) extends Action

case class SetJustRuleResults(ruleResults: List[RuleResult]) extends Action

case class SetPreviousWithResults(ruleResults: List[RuleResult], d2wContext: PageContext) extends Action


case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: EOEntity, task: String) extends Action

case class SetupQueryPageForEntity(entityName: String) extends Action

case object SwithDebugMode extends Action

case class FetchShowD2WDebugButton(d2wContext: PageContext) extends Action

case class SetDebugConfiguration(debugConf: DebugConf, d2wContext: D2WContext) extends Action

object EOCacheUtils {



  //for sorting list of eos
  def sortEOS(eos: List[EOContaining], sortOrderingOpt: Option[EOSortOrdering], cache: EOCache): List[EOContaining] = {
    sortOrderingOpt match {
      case Some(sortOrdering) =>
        val key = sortOrdering.key
        val isAscending = EOSortOrdering.isAscending(sortOrdering)
        val sortedEOs = eos.sortWith((a, b) => compareEOs(a, b, key, isAscending, cache))
        sortedEOs
      case None =>
        eos
    }
  }

  def compareEOs(a: EOContaining, b: EOContaining, propKey: String, isAsc: Boolean, cache: EOCache): Boolean = {
    val va = a.valueForKey(propKey).get
    val vb = b.valueForKey(propKey).get

    val isIntegerVa = va match {
      case IntValue(va) => true
      case _ => false
    }

    val isIntegerVb = vb match {
      case IntValue(vb) => true
      case _ => false
    }

    if(isIntegerVa && isIntegerVb){
      val v1 = EOValue.juiceInt(va)
      val v2 = EOValue.juiceInt(vb)
      if(isAsc){
        v1 < v2
      }else{
        v1 > v2
      }
    } else{
      val v1 = EOCacheUtils.juiceStringAtKeyPath(a, propKey, cache)
      val v2 = EOCacheUtils.juiceStringAtKeyPath(b, propKey, cache)
      if (isAsc) {
        v1 < v2
      } else {
        v1 > v2
      }
    }
  }

  def eoValueForKeyPath(eoContaining: EOContaining, keyPath: String, cache: EOCache): Option[EOValue] = {
    val eo = eoContaining.eo
    if (eo.keys.contains(keyPath)) {
      eoContaining.valueForKey(keyPath)
    } else {
      if (keyPath.contains(".")) {

        // currently supporting only a key path of 2 elements maximum
        val propertyNameList: List[String] = keyPath.split("\\.").map(_.trim).toList
        val firstKeyPathElement = propertyNameList.head
        if (eo.values.contains(firstKeyPathElement)) {
          val destinationEOValue = eoContaining.valueForKey(firstKeyPathElement)
          destinationEOValue match {
            case Some(ObjectValue(destinationEOPk)) =>
              val sourceEOEntity = EOModelUtils.entityNamed(cache.eomodel.get, eo.entityName).get
              val relationshipOpt = EORelationship.relationshipNamed(sourceEOEntity.relationships, firstKeyPathElement)
              relationshipOpt match {
                case Some(relationship) =>
                  val destinationEntityName = relationship.destinationEntityName
                  val destinationEOContainerOpt = outOfCacheEOUsingPk(cache, destinationEntityName, destinationEOPk)
                  destinationEOContainerOpt match {
                    case Some(destinationEOContainer) =>
                      val destinationEO = destinationEOContainer
                      destinationEO.valueForKey(propertyNameList(1))
                    case None => None
                  }
                case None =>
                  None
              }
            case _ =>
              None
          }
        } else {
          None
        }
      } else {
        None
      }
    }
  }


  def juiceStringAtKeyPath(eoContaining: EOContaining, keyPath: String, cache: EOCache): String = {
    val eo = eoContaining.eo
    val eoValueOpt = eoValueForKeyPath(eoContaining, keyPath, cache)
    eoValueOpt match {
      case Some(eoValue) => EOValue.juiceString(eoValue)
      case None => ""
    }
  }

  def refreshedEOMap(eos: List[EOContaining]): Map[EOPk, EOContaining] = eos.map(eo => {
    val pk = eo.eo.pk
    Some((pk, eo))
  }).flatten.toMap


  def readyEODBCacheEntityElement(eos: List[EOContaining]): EODBCacheEntityElement = {
    // we create a Map with eo and id
    // From eos to update, we create a map of key ->
    val refreshedEOs = refreshedEOMap(eos)
    EODBCacheEntityElement(Ready(refreshedEOs))
  }

  /*def memCacheEntityElement(eos: List[EO]): EOMemCacheEntityElement = {
  val refreshedEOs = refreshedEOMap(eos)
  EOMemCacheEntityElement(refreshedEOs)
}*/


  def updatedEOMap(entityMap: Map[EOPk, EOContaining], updatedEOSubset: List[EOContaining]): Map[EOPk, EOContaining] = {
    val refreshedEOs = refreshedEOMap(updatedEOSubset)
    // work with new and eo to be updated
    val refreshedPks = refreshedEOs.keySet
    val existingPks = entityMap.keySet

    val newPks = refreshedPks -- existingPks

    // Iterate on eos
    val newAndUpdateMap = refreshedPks.map(id => {
      //log.finest("Refresh " + entityName + "[" + id + "]")
      val refreshedEO = refreshedEOs(id)
      //log.finest("Refreshed " + refreshedEO)

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
    entityMap ++ newAndUpdateMap
  }


  //EOValueUtils.pk(eo)
  def newUpdatedEntityCache(entityName: String, cache: Map[String, EOCacheEntityElement], entityCacheElement: EOCacheEntityElement): Map[String, EOCacheEntityElement] = {
    cache + (entityName -> entityCacheElement)
  }


  // Returns None if nothing registered in the cache for that entityName
  def dbEOsForEntityNamed(cache: EOCache, entityName: String): Option[List[EOContaining]] = {
    val eoCacheEntityElementOpt = eoCacheEntityElementForEntityNamed(cache.eos, entityName)
    eoCacheEntityElementOpt match {
      case Some(eoCacheEntityElement) =>
        eoEntityMapOpt(eoCacheEntityElement) match {
          case Some(entityMap) =>
            Some(entityMap.values.toList)
          case None =>
            None
        }
      case None =>
        None
    }
  }


  def eoCacheEntityElementEos(eoCacheEntityElement: EOCacheEntityElement): List[EOContaining] =
    eoEntityMap(eoCacheEntityElement).values.toList

  def eoEntityMap(eoCacheEntityElement: EOCacheEntityElement): Map[EOPk, EOContaining] =
    eoEntityMapOpt(eoCacheEntityElement) match {
      case Some(entityMap) =>
        entityMap
      case None =>
        Map()
    }


  def eoEntityMapOpt(eoCacheEntityElement: EOCacheEntityElement): Option[Map[EOPk, EOContaining]] =
    eoCacheEntityElement match {
      case EODBCacheEntityElement(data, _) =>
        if (data.isEmpty) None else Some(data.get)
      case EOMemCacheEntityElement(data) =>
        Some(data)
    }


  def eoCacheEntityElementForEntityNamed(cache: Map[String, EOCacheEntityElement], entityName: String) = if (cache.contains(entityName)) Some(cache(entityName)) else None

  def allEOsForEntityNamed(cache: EOCache, entityName: String): List[EOContaining] = {
    val entityElements = List(
      eoCacheEntityElementForEntityNamed(cache.insertedEOs, entityName),
      eoCacheEntityElementForEntityNamed(cache.eos, entityName)
    )
    entityElements.flatten.flatMap(eoCacheEntityElementEos(_))
  }

  /*
def objectsWithFetchSpecification(eos: Map[String, Pot[Map[EOPk,EO]]],fs: EOFetchSpecification) : Option[List[EO]] = {
  val entityNameEOs = dbEOsForEntityNamed(eos,EOFetchSpecification.entityName(fs))
  //log.finest("EOCacheUtils.objectsWithFetchSpecification : " + entityNameEOs)
  entityNameEOs match {
    case Some(eos) =>
      Some(EOFetchSpecification.objectsWithFetchSpecification(eos,fs))
    case _ => None
  }
}*/

  def outOfCacheEOsUsingPkFromEOs(cache: EOCache, entityName: String, eos: List[EOContaining]): List[EOContaining] = {
    eos.map(eo => outOfCacheEOUsingPkFromEO(cache, entityName, eo)).flatten
  }


  def objectsFromAllCachesWithFetchSpecification(cache: EOCache, fs: EOFetchSpecification): List[EOContaining] = {
    val entityName = EOFetchSpecification.entityName(fs)
    val eos = allEOsForEntityNamed(cache, entityName)
    EOFetchSpecification.objectsWithFetchSpecification(eos, fs)
  }


  def outOfCacheEOUsingPkFromEO(cache: EOCache, entityName: String, eo: EOContaining): Option[EOContaining] = {
    outOfCacheEOUsingPk(cache, entityName, eo.eo.pk)
  }

  def outOfCacheEOUsingPks(cache: EOCache, entityName: String, pks: List[EOPk]): Seq[EOContaining] = {
    pks.map(pk => outOfCacheEOUsingPk(cache, entityName, pk)).flatten
  }

  def outOfCacheEOUsingPk(eoCache: EOCache, entityName: String, pk: EOPk): Option[EOContaining] = {
    val isInMemory = EOValue.isNew(pk)
    val cache = if (isInMemory) eoCache.insertedEOs else eoCache.eos
    val eoCacheEntityElementOpt = eoCacheEntityElementForEntityNamed(cache, entityName)
    eoCacheEntityElementOpt match {
      case Some(eoCacheEntityElement) =>
        val entityMap = eoEntityMap(eoCacheEntityElement)
        if (entityMap.contains(pk)) Some(entityMap(pk)) else None

      case None =>
        None
    }
  }


  def dbEntityMapForEntityNamed(eoCache: EOCache, entityName: String): Option[Map[EOPk, EOContaining]] = {
    entityMapForEntityNamed(eoCache.eos, entityName)
  }

  def memEntityMapForEntityNamed(eoCache: EOCache, entityName: String): Option[Map[EOPk, EOContaining]] = {
    entityMapForEntityNamed(eoCache.insertedEOs, entityName)
  }

  def entityMapForEntityNamed(cache: Map[String, EOCacheEntityElement], entityName: String): Option[Map[EOPk, EOContaining]] = {
    val eoCacheEntityElementOpt = eoCacheEntityElementForEntityNamed(cache, entityName)
    eoCacheEntityElementOpt match {
      case Some(eoCacheEntityElement) =>
        Some(EOCacheUtils.eoEntityMap(eoCacheEntityElement))
      case None => None
    }
  }

  // We speak DB Cache here
  def updatedDBCacheByDeletingEO(eoCache: EOCache, deletedEOContaining: EOContaining): EOCache = {
    val deletedEO = deletedEOContaining.eo
    val entityName = deletedEO.entityName
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, deletedEO.entityName)
    entitMapOpt match {
      case Some(entityMap) =>
        //val eoPk = EOValue.pk(value.eomodel.get, deletedEO).get
        val eoPk = deletedEO.pk
        val newEntityMap = entityMap - eoPk
        updatedCacheForDb(eoCache, entityName, newEntityMap)
      case None =>
        // should never happen
        eoCache
    }
  }

  def updatedCacheForDb(eoCache: EOCache, entityName: String, entityMap: Map[EOPk, EOContaining]) = {
    val newCache = newUpdatedEntityCache(entityName, eoCache.eos, EODBCacheEntityElement(Ready(entityMap)))
    eoCache.copy(eos = newCache)
  }

  def updatedCacheForMem(eoCache: EOCache, entityName: String, entityMap: Map[EOPk, EOContaining]) = {
    val newCache = newUpdatedEntityCache(entityName, eoCache.insertedEOs, EOMemCacheEntityElement(entityMap))
    eoCache.copy(insertedEOs = newCache)
  }


  def updatedDBCacheWithEO(eoCache: EOCache, eoContaining: EOContaining): EOCache = {
    val eo = eoContaining.eo
    val entityName = eo.entityName
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, eo.entityName)
    val eoPk = eo.pk

    val innerMap = Map(eoPk -> eoContaining)

    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        entityMap ++ innerMap
      case None =>
        innerMap
    }
    updatedCacheForDb(eoCache, entityName, newEntityMap)
  }


  def updatedMemCacheWithEOsForEntityNamed(eoCache: EOCache, eos: List[EOContaining], entityName: String): EOCache = {
    val entitMapOpt = memEntityMapForEntityNamed(eoCache, entityName)
    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        EOCacheUtils.updatedEOMap(entityMap, eos)
      case None =>
        refreshedEOMap(eos)
    }
    updatedCacheForMem(eoCache, entityName, newEntityMap)
  }

  def updatedDBCacheWithEOsForEntityNamed(eoCache: EOCache, eos: List[EOContaining], entityName: String): EOCache = {
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, entityName)
    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        EOCacheUtils.updatedEOMap(entityMap, eos)
      case None =>
        refreshedEOMap(eos)
    }
    updatedCacheForDb(eoCache, entityName, newEntityMap)
  }


  def removeEOFromCache(eo: EO, entityCache: Map[EOPk, EOContaining]): Map[EOPk, EOContaining] = {
    val entityName = eo.entityName
    val id = eo.pk
    val updatedEntityCache = entityCache - id
    updatedEntityCache
  }

  def updatedMemCacheByRemovingEO(eoCache: EOCache, eoContaining: EOContaining): EOCache = {
    val eo = eoContaining.eo
    val entityName = eo.entityName
    val entitMapOpt = memEntityMapForEntityNamed(eoCache, entityName)
    entitMapOpt match {
      case Some(entityMap) =>
        val newEntityMap = removeEOFromCache(eo, entityMap)
        updatedCacheForMem(eoCache, entityName, newEntityMap)
      case None =>
        eoCache
    }
  }

  def updatedCachesForSavedEO(eoCache: EOCache, eoContaining: EOContaining, memEO: Option[EOContaining]): EOCache = {
    // Adjust the insertedEOs cache
    val eo = eoContaining.eo
    val entityName = eo.entityName
    val newCache = if (memEO.isDefined) updatedMemCacheByRemovingEO(eoCache, memEO.get) else eoCache
    D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | removed if new  " + memEO)
    D2SpaLogger.logfinest(entityName, "CacheHandler | SavedEO | register eo  " + eo)

    // Adjust the db cache
    updatedDBCacheWithEO(newCache, eoContaining)
  }

  def updatedMemCacheWithEO(eoCache: EOCache, eoContaining: EOContaining): EOCache = {
    val eo = eoContaining.eo
    val entityName = eo.entityName
    updatedMemCacheWithEOsForEntityNamed(eoCache, List(eoContaining), entityName)
  }


  def newMemIdWithLastId(memID: Option[Int]) = {
    memID match {
      case Some(min) => min - 1
      case None => -1
    }
  }

  def newEOWithLastMemID(entityName: String, memID: Option[Int]) = {
    val newMemID = newMemIdWithLastId(memID)
    val newPk = EOPk(List(newMemID))
    val eo = EO(entityName, List.empty[String], List.empty[EOValue], pk = newPk, saved = false)
    EOContainer(eo)
  }

  def updatedMemCacheByCreatingNewEOForEntityNamed(eoCache: EOCache, entityName: String): (EOCache, EOContaining) = {
    val entitMapOpt = memEntityMapForEntityNamed(eoCache, entityName)
    val entityMap = entitMapOpt match {
      case Some(entityMap) => entityMap
      case None => Map.empty[EOPk, EOContaining]
    }
    val existingPks = entityMap.keySet.map(_.pks.head)
    val lastMemIDOpt = if (existingPks.isEmpty) None else Some(existingPks.min)
    val newEOContainer = newEOWithLastMemID(entityName, lastMemIDOpt)
    val newEO = newEOContainer.eo
    val newPk = newEO.pk

    val newEntityMap = entityMap + (newPk -> newEOContainer)

    (updatedCacheForMem(eoCache, entityName, newEntityMap), newEOContainer)
  }


}

