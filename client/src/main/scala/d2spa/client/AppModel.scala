package d2spa.client

import diode._
import diode.data.{Pot, _}
import diode.util._
import d2spa.shared.{D2WContextFullFledged, EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.EOCacheUtils.eoEntityMapOpt
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.shared.WebSocketMessages.RuleToFire
import jdk.nashorn.internal.ir.PropertyKey
/**
  * Created by dschoen on 01.05.17.
  */





sealed trait RulesContainer {
  def ruleResults: List[RuleResult]
}

case class AppConfiguration(serverAppConf: DebugConf = DebugConf(true), isDebugMode: Boolean = false, fetchMenus: Boolean = true, socketReady: Boolean = false)

case class PageConfigurationRuleResults(override val ruleResults: List[RuleResult] = List(), metaDataFetched: Boolean = false, properties: Map[String,PropertyRuleResults] = Map()) extends RulesContainer
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
case class EODBCacheEntityElement(data : Pot[Map[EOPk,EO]]) extends EOCacheEntityElement
case class EOMemCacheEntityElement(data : Map[EOPk,EO]) extends EOCacheEntityElement


case class EOCache(eomodel: Pot[EOModel],
                   eos: Map[String, EOCacheEntityElement],
                   insertedEOs: Map[String,EOCacheEntityElement])


case class DebugConf(showD2WDebugButton: Boolean)
case class SendingAction(action: Action) extends Action

// define actions
case object ShowBusyIndicator extends Action
case object HideBusyIndicator extends Action
case class SearchWithBusyIndicator(entityName: String) extends Action

case object SocketReady extends Action
case class InitAppSpecificClient(d2wContext: D2WContextFullFledged) extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus, d2wContext: D2WContextFullFledged) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class UpdateRefreshEOInCache(eos: Seq[EO], d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class UpdateEOInCache(eo:EO) extends Action
case object FetchEOModel extends Action
case class FetchEOModelAndMenus(d2wContext: D2WContextFullFledged) extends Action


case class SetEOModelThenFetchMenu(eomodel: EOModel, d2wContext: D2WContextFullFledged) extends Action
case class SetEOModel(eomodel: EOModel) extends Action
case class FetchMenu(d2wContext: D2WContextFullFledged) extends Action

case class FetchedObjectsForEntity(entityName: String, eos: Seq[EO], ruleResults: Option[List[RuleResult]]) extends Action
case class CompletedEO(d2wContext: D2WContextFullFledged, eo: EO, ruleResults: Option[List[RuleResult]]) extends Action
case class SetPageForSocketReady(d2wContext: D2WContext) extends Action

case class RefreshedEOs(eos: Seq[EO])

case class InitMetaData(d2wContext: D2WContext) extends Action
case class InitMetaDataForList(entityName: String) extends Action
//case class SetPageForTaskAndEntity(d2wContext: D2WContext) extends Action
case class CreateEO(entityName:String) extends Action

case class SetMetaData(d2wContext: D2WContext, metaData: EntityMetaData) extends Action
//case class SetMetaDataForMenu(d2wContext: D2WContext, metaData: EntityMetaData) extends Action

case class NewAndRegisteredEO(d2wContext: D2WContext) extends Action
case class RegisteredExistingEO(d2wContext: D2WContext) extends Action


case class NewEOCreated(eo: EO, d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class NewEOCreatedForEdit(eo: EO) extends Action

case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case object SetPreviousPage extends Action

case class PrepareEODisplay(d2wContext: D2WContext) extends Action
case class PrepareEditPage(d2wContext: D2WContext) extends Action


case class PrepareEODisplayRules(d2wContext: D2WContext, cache: EOCache, needsHydration: Boolean) extends Action

case class RegisterPreviousPage(d2wContext: D2WContext) extends Action
case class RegisterPreviousPageAndSetPage(d2wContext: D2WContext) extends Action

case class SetPage(d2WContext: D2WContext) extends Action
case class UpdateCurrentContextWithEO(eo: EO) extends Action
case object InitMenuSelection extends Action

case object InitAppModel extends Action
case class ShowResults(fs: EOFetchSpecification) extends Action

case class SelectMenu(entityName: String) extends Action
case class Save(entityName: String, eo: EO) extends Action
case class SavingEO(d2wContext: D2WContextFullFledged, eo: EO, ruleResults: Option[List[RuleResult]]) extends Action
case class SaveNewEO(entityName: String, eo: EO) extends Action

case class UpdateQueryProperty(entityName: String, queryValue: QueryValue) extends Action
case class ClearQueryProperty(entityName: String, propertyName: String) extends Action
case class UpdateEOValueForProperty(eo: EO, d2wContext: D2WContext, value: EOValue) extends Action

case class SearchAction(entityName: String) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(fs: EOFetchSpecification, eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class SavedEO(dw2Context: D2WContext) extends Action
case class SavedEOWithResults(fromTask: String, eo: EO, dw2Context: D2WContext, ruleResults: Option[List[RuleResult]]) extends Action

case class DeleteEO(fromTask: String, eo: EO) extends Action
case class DeleteEOFromList(eo: EO) extends Action
case class DeletingEO(eo: EO) extends Action
case class EditEO(dw2Context: D2WContext) extends Action
case class EditEOWithResults(fromTask: String, eo: EO, dw2Context: D2WContext, ruleResults: Option[List[RuleResult]]) extends Action
case class InspectEO(fromTask: String, eo: EO, isOneRecord: Boolean = false) extends Action

case class SaveError(eo: EO) extends Action
object ListEOs extends Action
case class DeletedEO(eo:EO) extends Action
case class UpdateEOsForEOOnError(eo:EO) extends Action

trait D2WAction extends diode.Action
case class FireRule(rhs: D2WContext, key: String) extends D2WAction

// Typical case:
// key : componentName
// keysSubstrate: displayPropertyKeys either defined by a rule result or a rule to fire
// In that case the FireRules will trigger RuleToFire action to the server for as many entries in dipslayPropertyKeys
case class FireRules(propertyKeys: List[String], rhs: D2WContext, key: String) extends D2WAction
case class Hydration(drySubstrate: DrySubstrate,  wateringScope: WateringScope) extends D2WAction
case class CreateMemID(entityName: String) extends D2WAction
case class FetchMetaData(d2wContext: D2WContext) extends Action

case class FireActions(d2wContext: D2WContext, actions: List[D2WAction]) extends Action


object Hydration {

  def toFault(eo:EO) = EOFault(eo.entityName, eo.pk)

  // case class DrySubstrate(eosAtKeyPath: Option[EOsAtKeyPath] = None, eo: Option[EOFault] = None, fetchSpecification: Option[EOFetchSpecification] = None)
  def entityName(drySubstrate: DrySubstrate) = drySubstrate match {
    case DrySubstrate(Some(eosAtKeyPath), _ , _) => eosAtKeyPath.eo.entityName
    case DrySubstrate(_, Some(eoFault) , _) => eoFault.entityName
    case DrySubstrate(_, _ , Some(fs)) => EOFetchSpecification.entityName(fs)
  }

  def isEOHydrated(cache: EOCache, entityName: String, pk : EOPk, propertyKeys: List[String]) : Boolean = {
    val eoOpt = EOCacheUtils.outOfCacheEOUsingPk(cache, entityName, pk)
    eoOpt match {
      case Some(eo) =>
        val valuesKeys = eo.values
        val anyMissingPropertyKey = propertyKeys.find(p => !valuesKeys.contains(p))
        anyMissingPropertyKey.isEmpty
      case None => false
    }

  }

  // EOs are supposed to exists in cache but not hydrated correctly in scope
  def isHydratedForPropertyKeys(eomodel: EOModel, cache: EOCache, drySubstrate: DrySubstrate, propertyKeys: List[String]): Boolean = {
    drySubstrate match {
      case DrySubstrate(_, Some(eoFault), _) =>
        isEOHydrated(cache,eoFault.entityName, eoFault.pk, propertyKeys)
      case DrySubstrate(Some(eoakp), _, _) =>
        //log.finest("Hydration DrySubstrate " + eoakp.eo.entity.name + " for key " + eoakp.keyPath)
        val eovalueOpt = EOValue.valueForKey(eoakp.eo, eoakp.keyPath)
        //log.finest("Hydration DrySubstrate valueForKey " + eovalueOpt)

        eovalueOpt match {
          case Some(eovalue) =>
            eovalue match {
              case ObjectsValue(pks) =>
                //log.finest("NVListComponent render pks " + pks)
                val destinationEntityName = eoakp.destinationEntityName
                val nonHydrated = pks.find(pk => !isEOHydrated(cache, destinationEntityName, pk, propertyKeys))
                log.finest("AppModel | Hydration | isHydratedForPropertyKeys " + destinationEntityName + " at path: " + eoakp.keyPath + " any non hydrated " + nonHydrated)
                nonHydrated.isEmpty

              case _ => false
            }
          case _ => false
        }
      case DrySubstrate(_, _, Some(fs)) =>
        val eos = EOCacheUtils.objectsFromAllCachesWithFetchSpecification(cache,fs)
        val nonHydrated = eos.find(eo => !isEOHydrated(cache, eo.entityName, eo.pk, propertyKeys))
        !nonHydrated.isDefined
    }
  }

}

object QueryOperator {
  val Match = "Match"
  val Min = "Min"
  val Max = "Max"
}


// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: EOValue, operator: String)





object QueryValue {
  val operatorByQueryOperator = Map(
    QueryOperator.Max -> NSSelector.QualifierOperatorLessThanOrEqualTo,
    QueryOperator.Min -> NSSelector.QualifierOperatorGreaterThanOrEqualTo,
    QueryOperator.Match -> NSSelector.QualifierOperatorEqual
  )

  def qualifierFromQueryValues(queryValues: List[QueryValue]) : Option[EOQualifier] = {
    val qualifiers = queryValues.map(qv => {
      log.finest("QueryValue " + qv)
      log.finest("QueryValue operatorByQueryOperator(qv.operator): " + operatorByQueryOperator(qv.operator))
      EOKeyValueQualifier(qv.key,operatorByQueryOperator(qv.operator),qv.value)
    })
    if (qualifiers.isEmpty) None else Some(EOAndQualifier(qualifiers))
  }

  def size(queryValue: QueryValue) = EOValue.size(queryValue.value)
}


//implicit val fireActionPickler = CompositePickler[FireAction].

// The D2WContext will contains also the queryValues
// it should contain everything needed to redisplay a page
case class D2WContext(entityName: Option[String],
                      task: Option[String],
                      previousTask: Option[D2WContext] = None,
                      //pageCounter: Int = 0,
                      eo: Option[EO] = None,
                      queryValues: Map[String, QueryValue] = Map(),
                      dataRep: Option[DataRep] = None,
                      propertyKey:  Option[String] = None,

                      // Either
                      // - defined by a rule not yet fired (RuleFault). It is a action which is before in the list
                      // - a value
                      // - None
                      pageConfiguration: PotFiredKey = PotFiredKey(Right(None))
                      )

// Right = Some
// here right is an result which could be Some or None
// left is not yet a result because it is a rule to be fired
case class PotFiredKey (value: Either[FireRule, Option[String]])
case class PotFiredRuleResult (value: Either[FireRule, RuleResult])

case class DataRep (fetchSpecification: Option[EOFetchSpecification] = None, eosAtKeyPath: Option[EOsAtKeyPath] = None)

object NSSelector {
  val QualifierOperatorEqual = "QualifierOperatorEqual"
  val QualifierOperatorNotEqual = "QualifierOperatorNotEqual"
  val QualifierOperatorLessThan = "QualifierOperatorLessThan"
  val QualifierOperatorGreaterThan = "QualifierOperatorGreaterThan"
  val QualifierOperatorGreaterThanOrEqualTo = "QualifierOperatorGreaterThanOrEqualTo"
  val QualifierOperatorLessThanOrEqualTo = "QualifierOperatorLessThanOrEqualTo"
  val QualifierOperatorContains = "QualifierOperatorContains"
  val QualifierOperatorLike = "QualifierOperatorLike"
  val QualifierOperatorCaseInsensitiveLike = "QualifierOperatorCaseInsensitiveLike"
}

object EOQualifierType {
  val EOAndQualifier = "EOAndQualifier"
  val EOOrQualifier = "EOOrQualifier"
  val EOKeyValueQualifier = "EOKeyValueQualifier"
  val EONotQualifier = "EONotQualifier"
}


case class KeysSubstrate(ruleResult: PotFiredRuleResult)


case class DrySubstrate(
                          eosAtKeyPath: Option[EOsAtKeyPath] = None,
                          eo: Option[EOFault] = None,
                          fetchSpecification: Option[EOFetchSpecification] = None
                       )
case class WateringScope(ruleResult: PotFiredRuleResult)
//case class EORefsDefinition()
case class EOsAtKeyPath(eo: EO, keyPath: String, destinationEntityName: String)
//case class RuleRawResponse(ruleFault: RuleFault, response: WSResponse)




object RuleUtils {

  def metaDataFetched(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext): Boolean  = {
    val ruleContainerOpt = RuleUtils.pageConfigurationRuleResultsForContext(ruleResults,d2wContext)
    ruleContainerOpt match {
      case Some(rulesContainer) => rulesContainer.metaDataFetched
      case None => false
    }
  }

  def potentialFireRule(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key: String) : Option[FireRule] = {
    val ruleResultOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, key)
    ruleResultOpt match {
      case Some(_) => None
      case None =>
        Some(FireRule(d2wContext, key))
    }
  }




  def potentialFireRules(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]],
                         d2wContext: D2WContext,
                         displayPropertyKeysRuleResultOpt: Option[RuleResult],
                         key: String) : Option[FireRules] = {

    displayPropertyKeysRuleResultOpt match {
      case Some(displayPropertyKeysRuleResult) =>
        val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(displayPropertyKeysRuleResultOpt)
        if (displayPropertyKeys.size == 0) {
          None
        } else {
          val propertyD2WContext = d2wContext.copy(propertyKey = Some(displayPropertyKeys.head))
          val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, propertyD2WContext, key)
          ruleResultOpt match {
            case Some(_) => None
            case None =>
              Some(FireRules(displayPropertyKeys, d2wContext, key))
          }
        }
      case None => None
    }
  }

  /*
    RuleFault(
        D2WContext(
            Some(Project),Some(list),None,None,Map(),None,None,
            // Page Configuration
            Some(
                Left(
                    RuleFault(
                        D2WContext(
                            Some(Project),Some(list),None,None,Map(),
                            Some(
                                DataRep(None,
                                    Some(
                                        EOsAtKeyPath(
                                            EO(
                                              EOEntity(
                                                Customer,id,List(EORelationship(projects,Project))
                                              ),
                                              Map(
                                                acronym -> StringValue(Some(8)),
                                                name -> StringValue(Some(8)),
                                                id -> IntValue(Some(8)),
                                                address -> StringValue(Some(8)),
                                                projects -> ObjectsValue(Vector(1)),
                                                type -> StringValue(Some(Customer))
                                              ),8,None)
                                            ,projects
                                        )
                                    )
                                )
                            ),
                            None,
                            None
                        )
                        ,listConfigurationName
                    )
                )
            )
        )
        ,displayPropertyKeys
    )
   */

  /*def fireRuleFault(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], ruleFault: RuleFault): Option[RuleResult] = {
    val ruleKey = ruleFault.key
    val ruleRhs = D2WContextUtils.d2wContextByResolvingRuleFaults(ruleResults,ruleFault.rhs)
    RuleUtils.ruleResultForContextAndKey(ruleResults,ruleRhs,ruleKey)
  }*/




  def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {D2WContextUtils.isD2WContextEquals(r.rhs,rhs) && r.key.equals(key)})
  //def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {r.key.equals(key)})

  def ruleStringValueForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): Option[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleStringValueWithRuleResult(ruleResult)
  }

  def ruleBooleanValueForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): Boolean = {
    val booleanAsStringOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, key)
    booleanAsStringOpt match {
      case Some(boolVal) => boolVal.equals("true")
      case None => false
    }
  }

  def ruleListValueForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): List[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    RulesUtilities.ruleListValueWithRuleResult(ruleResult)
  }

  def pageConfigurationWithRuleResult(propertyKeyOpt: Option[String],ruleResult: RuleResult) = {
    propertyKeyOpt match {
      case Some(propertyKey) => {
        PageConfigurationRuleResults(properties = Map(propertyKey -> PropertyRuleResults(ruleResults = List(ruleResult))))
      }
      case _ => {
        PageConfigurationRuleResults(ruleResults = List(ruleResult))
      }
    }
  }


  def registerRuleResult(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], ruleResult: RuleResult): Map[String,Map[String,Map[String,PageConfigurationRuleResults]]] = {
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
                existingPropertyRuleResults.copy(ruleResults = ruleResult :: existingPropertyRuleResults.ruleResults )
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
          pageConfigurationWithRuleResult(d2wContext.propertyKey,ruleResult)
        }
        val flaggedPageConfigurationRuleResults = if (ruleResult.key.equals(RuleKeys.displayNameForEntity)) pageConfigurationRuleResults.copy(metaDataFetched = true) else pageConfigurationRuleResults
        existingPageConfigurationSubTree + (pageConfiguration -> flaggedPageConfigurationRuleResults)
      } else {
        val pageConfigurationRuleResults = pageConfigurationWithRuleResult(d2wContext.propertyKey,ruleResult)
        val flaggedPageConfigurationRuleResults = if (ruleResult.key.equals(RuleKeys.displayNameForEntity)) pageConfigurationRuleResults.copy(metaDataFetched = true) else pageConfigurationRuleResults
        Map(pageConfiguration -> flaggedPageConfigurationRuleResults)
      }
      existingTaskSubTree + (task -> pageConfigurationSubTree)
    } else {
      val pageConfigurationRuleResults = pageConfigurationWithRuleResult(d2wContext.propertyKey,ruleResult)
      val flaggedPageConfigurationRuleResults = if (ruleResult.key.equals(RuleKeys.displayNameForEntity)) pageConfigurationRuleResults.copy(metaDataFetched = true) else pageConfigurationRuleResults

      Map(task -> Map(pageConfiguration -> flaggedPageConfigurationRuleResults))
    }
    ruleResults + (entityName -> taskSubTree)
  }

  def pageConfigurationRuleResultsForContext(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext): Option[PageConfigurationRuleResults] = {
    val entityName = d2wContext.entityName.get
    val task = d2wContext.task.get
    val pageConfiguration: String = if (d2wContext.pageConfiguration.value.isRight) {
      d2wContext.pageConfiguration.value.right.get match {
        case Some(pc) => pc
        case None => PageConfiguration.NoPageConfiguration
      }
    } else PageConfiguration.NoPageConfiguration
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

  def ruleContainerForContext(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext): Option[RulesContainer] = {
    val pageConfigurationRuleResultsOpt = pageConfigurationRuleResultsForContext(ruleResults,d2wContext)
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

  def ruleResultForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): Option[RuleResult] = {
    val ruleContainerOpt = ruleContainerForContext(ruleResults,d2wContext)
    ruleContainerOpt match {
      case Some(rulesContainer) => {
        ruleResultForContextAndKey(rulesContainer.ruleResults,d2wContext,key)
      }
      case _ => None
    }
  }


  def ruleStringValueWithRuleResult(ruleResultOpt: Option[RuleResult]) = {
    ruleResultOpt match {
      case Some(ruleResult) => ruleResult.value.stringV
      case _ => None
    }
  }

  def existsRuleResultForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String) =
    ruleResultForContextAndKey(ruleResults, d2wContext, key).isDefined


}

object D2WContextUtils {


  def queryValueForKey(d2wContext: D2WContext, key: String) = {
    val queryValues = d2wContext.queryValues
    if (queryValues.contains(key)) Some(queryValues(key).value) else None
  }

  def queryValueAsStringForKey(d2wContext: D2WContext, key: String) = {
    val value = D2WContextUtils.queryValueForKey(d2wContext, key)
    val strValue = value match {
      case Some(StringValue(str)) => str
      case _ => ""
    }
  }

  def convertToPotFiringKey(potFiredKey: PotFiredKey): PotFiringKey = {
    potFiredKey.value match {
      case Right(b) => PotFiringKey(Right(b))
      case Left(fireRule) => {
        val newRhs = D2WContextUtils.convertFromD2WContextToFiringD2WContext(fireRule.rhs)
        PotFiringKey(Left(RuleToFire(newRhs,fireRule.key)))
      }
    }
  }




  def convertFromD2WContextToFiringD2WContext(d2wContext: D2WContext): FiringD2WContext = {
    FiringD2WContext(
        d2wContext.entityName,
        d2wContext.task,
        d2wContext.propertyKey,
        convertToPotFiringKey(d2wContext.pageConfiguration)
    )
  }

  def convertD2WContextToFullFledged(d2wContext: D2WContext): D2WContextFullFledged = {
    //log.finest("D2WContextUtils.convertD2WContextToFullFledged : " + d2wContext.pageConfiguration)
    /*if(d2wContext.pageConfiguration.isDefined) {
      log.finest("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get " + d2wContext.pageConfiguration.get)
      log.finest("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get.right " + d2wContext.pageConfiguration.get.right)
      log.finest("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get.right.get " + d2wContext.pageConfiguration.get.right.get)

    }*/

    D2WContextFullFledged(
      d2wContext.entityName,
      d2wContext.task,
      d2wContext.propertyKey,
      d2wContext.pageConfiguration.value.right.get,
      d2wContext.eo
    )
  }

  def convertFullFledgedToD2WContext(d2wContext: D2WContextFullFledged) = D2WContext(
    d2wContext.entityName,
    d2wContext.task,
    None,
    d2wContext.eo,
    Map(),
    None,
    d2wContext.propertyKey,
    PotFiredKey(Right(d2wContext.pageConfiguration))
  )


  def isD2WContextEquals(a: D2WContextFullFledged, b: D2WContext): Boolean  = {
    if (!a.entityName.equals(b.entityName)) return false
    if (!a.task.equals(b.task)) return false
    if (!a.propertyKey.equals(b.propertyKey)) return false
    if (!(b.pageConfiguration.value.isRight && a.pageConfiguration.equals(b.pageConfiguration.value.right.get))) return false
    return true
  }

}


case class SetRuleResults(ruleResults: List[RuleResult], d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class SetJustRuleResults(ruleResults: List[RuleResult]) extends Action
case class SetPreviousWithResults(ruleResults: List[RuleResult], d2wContext: D2WContext) extends Action



case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: EOEntity, task: String) extends Action
case class SetupQueryPageForEntity(entityName: String) extends Action

case object SwithDebugMode extends Action
case class FetchShowD2WDebugButton(d2wContext: D2WContext) extends Action
case class SetDebugConfiguration(debugConf: DebugConf, d2wContext: D2WContextFullFledged) extends Action

object EOCacheUtils {

  def removeEOFromCache(eo: EO, eos: Map[String, Map[EOPk, EO]]): Map[String, Map[EOPk, EO]] = {
    val entityName = eo.entityName
    val id = eo.pk
    val entityCache = eos(entityName)
    val updatedEntityCache = entityCache - id
    val updatedCache = eos + (entityName -> updatedEntityCache)
    updatedCache
  }

  def refreshedEOMap(eos: List[EO]): Map[EOPk, EO] = eos.map(eo => {
    val pk = eo.pk
    Some((pk, eo))
  }).flatten.toMap


  def readyEODBCacheEntityElement(eos: List[EO]): EODBCacheEntityElement = {
    // we create a Map with eo and id
    // From eos to update, we create a map of key ->
    val refreshedEOs = refreshedEOMap(eos)
    EODBCacheEntityElement(Ready(refreshedEOs))
  }

  /*def memCacheEntityElement(eos: List[EO]): EOMemCacheEntityElement = {
    val refreshedEOs = refreshedEOMap(eos)
    EOMemCacheEntityElement(refreshedEOs)
  }*/


  def updatedEOMap(entityMap: Map[EOPk, EO], updatedEOSubset: List[EO]): Map[EOPk, EO] = {
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
  def dbEOsForEntityNamed(cache: EOCache, entityName: String): Option[List[EO]] = {
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


  def eoCacheEntityElementEos(eoCacheEntityElement: EOCacheEntityElement): List[EO] =
    eoEntityMap(eoCacheEntityElement).values.toList

  def eoEntityMap(eoCacheEntityElement: EOCacheEntityElement): Map[EOPk, EO] =
    eoEntityMapOpt(eoCacheEntityElement) match {
      case Some(entityMap) =>
        entityMap
      case None =>
        Map()
    }


  def eoEntityMapOpt(eoCacheEntityElement: EOCacheEntityElement): Option[Map[EOPk, EO]] =
    eoCacheEntityElement match {
      case EODBCacheEntityElement(data) =>
        if (data.isEmpty) None else Some(data.get)
      case EOMemCacheEntityElement(data) =>
        Some(data)
    }



  def eoCacheEntityElementForEntityNamed(cache: Map[String, EOCacheEntityElement], entityName: String) = if (cache.contains(entityName)) Some(cache(entityName)) else None

  def allEOsForEntityNamed(cache: EOCache, entityName: String) : List[EO] = {
    val entityElements = List(
      eoCacheEntityElementForEntityNamed(cache.insertedEOs,entityName),
      eoCacheEntityElementForEntityNamed(cache.eos,entityName)
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

  def outOfCacheEOsUsingPkFromEOs(cache: EOCache, entityName: String, eos: List[EO]): List[EO] = {
    eos.map(eo => outOfCacheEOUsingPkFromD2WContextEO(cache,entityName,eo)).flatten
  }




  def objectsFromAllCachesWithFetchSpecification(cache: EOCache, fs: EOFetchSpecification): List[EO] = {
    val entityName = EOFetchSpecification.entityName(fs)
    val eos = allEOsForEntityNamed(cache,entityName)
    EOFetchSpecification.objectsWithFetchSpecification(eos,fs)
  }


  def outOfCacheEOUsingPkFromD2WContextEO(cache: EOCache, entityName: String, eo: EO): Option[EO] = {
    outOfCacheEOUsingPk(cache, entityName, eo.pk)
  }

  def outOfCacheEOUsingPks(cache: EOCache, entityName: String, pks: List[EOPk]): Seq[EO] = {
    pks.map(pk => outOfCacheEOUsingPk(cache, entityName, pk)).flatten
  }

  def outOfCacheEOUsingPk(eoCache: EOCache, entityName: String, pk: EOPk): Option[EO] = {
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


  def dbEntityMapForEntityNamed(eoCache : EOCache, entityName: String) : Option[Map[EOPk, EO]] = {
    entityMapForEntityNamed(eoCache.eos, entityName)
  }

  def memEntityMapForEntityNamed(eoCache : EOCache, entityName: String) : Option[Map[EOPk, EO]] = {
    entityMapForEntityNamed(eoCache.insertedEOs, entityName)
  }

  def entityMapForEntityNamed(cache : Map[String, EOCacheEntityElement], entityName: String) : Option[Map[EOPk, EO]] = {
    val eoCacheEntityElementOpt = eoCacheEntityElementForEntityNamed(cache, entityName)
    eoCacheEntityElementOpt match {
      case Some(eoCacheEntityElement) =>
        Some(EOCacheUtils.eoEntityMap(eoCacheEntityElement))
      case None => None
    }
  }

  // We speak DB Cache here
  def updatedDBCacheByDeletingEO(eoCache : EOCache, deletedEO: EO) : EOCache = {
    val entityName = deletedEO.entityName
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, deletedEO.entityName)
    entitMapOpt match {
      case Some(entityMap) =>
        //val eoPk = EOValue.pk(value.eomodel.get, deletedEO).get
        val eoPk = deletedEO.pk
        val newEntityMap = entityMap - eoPk
        updatedCacheForDb(eoCache,entityName,newEntityMap)
      case None =>
        // should never happen
        eoCache
    }
  }

  def updatedCacheForDb(eoCache : EOCache, entityName: String, entityMap: Map[EOPk, EO]) = {
    val newCache = newUpdatedEntityCache(entityName, eoCache.eos, EODBCacheEntityElement(Ready(entityMap)))
    eoCache.copy(eos = newCache)
  }

  def updatedCacheForMem(eoCache : EOCache, entityName: String, entityMap: Map[EOPk, EO]) = {
    val newCache = newUpdatedEntityCache(entityName, eoCache.insertedEOs, EOMemCacheEntityElement(entityMap))
    eoCache.copy(insertedEOs = newCache)
  }


  def updatedDBCacheWithEO(eoCache : EOCache, eo: EO) : EOCache = {
    val entityName = eo.entityName
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, eo.entityName)
    val eoPk = eo.pk

    val innerMap = Map(eoPk -> eo)

    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        entityMap ++ innerMap
      case None =>
        innerMap
    }
    updatedCacheForDb(eoCache,entityName,newEntityMap)
  }



  def updatedMemCacheWithEOsForEntityNamed(eoCache : EOCache, eos: List[EO], entityName: String): EOCache = {
    val entitMapOpt = memEntityMapForEntityNamed(eoCache, entityName)
    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        EOCacheUtils.updatedEOMap(entityMap, eos)
      case None =>
        refreshedEOMap(eos)
    }
    updatedCacheForMem(eoCache,entityName,newEntityMap)
  }

  def updatedDBCacheWithEOsForEntityNamed(eoCache : EOCache, eos: List[EO], entityName: String): EOCache = {
    val entitMapOpt = dbEntityMapForEntityNamed(eoCache, entityName)
    val newEntityMap = entitMapOpt match {
      case Some(entityMap) =>
        EOCacheUtils.updatedEOMap(entityMap, eos)
      case None =>
        refreshedEOMap(eos)
    }
    updatedCacheForDb(eoCache,entityName,newEntityMap)
  }




  def updatedCachesForSavedEO(eoCache : EOCache, eo: EO, isNewEO: Boolean) : EOCache = {
    // Adjust the insertedEOs cache
    val entityName = eo.entityName
    val newCache = if (isNewEO) updatedMemCacheWithEO(eoCache, eo) else eoCache
    D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | removed if new  " + isNewEO)
    D2SpaLogger.logfinest(entityName,"CacheHandler | SavedEO | register eo  " + eo)

    // Adjust the db cache
    updatedDBCacheWithEO(newCache, eo)
  }

  def updatedMemCacheWithEO(eoCache : EOCache, eo: EO) : EOCache = {
    val entityName = eo.entityName
    updatedMemCacheWithEOsForEntityNamed(eoCache, List(eo), entityName)
  }


  def updatedMemCacheByCreatingNewEOForEntityNamed(eoCache : EOCache, entityName: String): (EOCache, EO) = {
    val entitMapOpt = memEntityMapForEntityNamed(eoCache, entityName)
    val entityMap = entitMapOpt match {
      case Some(entityMap) => entityMap
      case None => Map.empty[EOPk, EO]
    }
    val existingPks = entityMap.keySet.map(_.pks.head)
    val newMemID = if (existingPks.isEmpty) -1 else existingPks.min - 1
    val newPk = EOPk(List(newMemID))
    val newEO = EO(entityName, List.empty[String], List.empty[EOValue], pk = newPk)
    val newEntityMap = entityMap + (newPk -> newEO)

    (updatedCacheForMem(eoCache,entityName,newEntityMap), newEO)
  }





}

