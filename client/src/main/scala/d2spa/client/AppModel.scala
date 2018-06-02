package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared.{EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.logger.log
import jdk.nashorn.internal.ir.PropertyKey
/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)

// , genericPart: GenericData, customPart: CustomData


case class CustomData()



case class MegaContent(showBusyIndicator: Boolean = false,   debugConfiguration: DebugConfiguration, menuModel: Pot[Menus], eomodel: Pot[EOModel], ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]],
                       cache: EOCache,
                       previousPage: Option[D2WContext]
                       )

sealed trait RulesContainer {
  def ruleResults: List[RuleResult]
}
case class DebugConfiguration(showD2WDebugButton: Boolean = false, isDebugMode: Boolean = false)


case class PageConfigurationRuleResults(override val ruleResults: List[RuleResult] = List(), metaDataFetched: Boolean = false, properties: Map[String,PropertyRuleResults] = Map()) extends RulesContainer
case class PropertyRuleResults(override val ruleResults: List[RuleResult] = List(), typeV: String = "stringV") extends RulesContainer
object PageConfiguration {
  val NoPageConfiguration = "NoPageConfiguration"
}

case class D2WContextEO(pk: Option[Int] = None, memID : Option[Int] = None)



case class EOCache(eos: Map[String, Map[Int,EO]],
                    insertedEOs: Map[String, Map[Int,EO]])




// define actions
case object ShowBusyIndicator extends Action
case object HideBusyIndicator extends Action
case class SearchWithBusyIndicator(entityName: String) extends Action

case object InitClient extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class UpdateRefreshEOInCache(eo:EO, d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class UpdateEOInCache(eo:EO) extends Action
case object FetchEOModel extends Action
case class SetEOModelThenFetchMenu(eomodel: EOModel) extends Action
case class SetEOModel(eomodel: EOModel) extends Action
case object FetchMenu extends Action

case class FetchedObjectsForEntity(eos: Seq[EO], d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class RefreshedEOs(eos: Seq[EO])

case class InitMetaData(entityName: String) extends Action
case class InitMetaDataForList(entityName: String) extends Action
//case class SetPageForTaskAndEntity(d2wContext: D2WContext) extends Action
case class CreateEO(entityName:String) extends Action

case class SetMetaData(d2wContext: D2WContext, metaData: EntityMetaData) extends Action
//case class SetMetaDataForMenu(d2wContext: D2WContext, metaData: EntityMetaData) extends Action

case class NewEOWithEOModel(eomodel: EOModel, d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class NewEOWithEOModelForEdit(entity: EOEntity) extends Action
case class NewEOWithEntityName(d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class NewEOWithEntityNameForEdit(entityName: String) extends Action
case class NewEOCreated(eo: EO, d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class NewEOCreatedForEdit(eo: EO) extends Action

case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case object SetPreviousPage extends Action
case class RegisterPreviousPage(d2WContext: D2WContext) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entityName: String) extends Action
case class Save(entityName: String, eo: EO) extends Action
case class SaveNewEO(entityName: String, eo: EO) extends Action

case class UpdateQueryProperty(entityName: String, queryValue: QueryValue) extends Action
case class ClearQueryProperty(entityName: String, propertyName: String) extends Action
case class UpdateEOValueForProperty(eo: EO, d2wContext: D2WContext, value: EOValue) extends Action

case class Search(entityName: String) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(entityName: String, eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class SavedEO(fromTask: String, eo: EO) extends Action
case class DeleteEO(fromTask: String, eo: EO) extends Action
case class DeleteEOFromList(fromTask: String, eo: EO) extends Action
case class EditEO(fromTask: String, eo: EO) extends Action
case class InspectEO(fromTask: String, eo: EO) extends Action

case class SaveError(eo: EO) extends Action
object ListEOs extends Action
case class DeletedEO(eo:EO) extends Action
case class UpdateEOsForEOOnError(eo:EO) extends Action

trait D2WAction extends diode.Action
case class FireRule(rhs: D2WContext, key: String) extends D2WAction
case class FireRules(keysSubstrate: KeysSubstrate, rhs: D2WContext, key: String) extends D2WAction
case class Hydration(drySubstrate: DrySubstrate,  wateringScope: WateringScope) extends D2WAction
case class CreateMemID(entityName: String) extends D2WAction
case class FetchMetaData(d2wContext: D2WContext) extends D2WAction

case class FireActions(d2wContext: D2WContext, actions: List[D2WAction]) extends Action


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
      log.debug("QueryValue " + qv)
      log.debug("QueryValue operatorByQueryOperator(qv.operator): " + operatorByQueryOperator(qv.operator))
      EOKeyValueQualifier(qv.key,operatorByQueryOperator(qv.operator),qv.value)
    })
    if (qualifiers.isEmpty) None else Some(EOAndQualifier(qualifiers))
  }
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
                      pageConfiguration: Option[Either[RuleFault,String]] = None,
                      pk : Option[Int] = None)

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


case class KeysSubstrate(ruleFault: Option[RuleFault] = None)


// A rule fault should be a FullFledged because it is a rule
// but the goal is to be used for watering the displayPropertyKeys but this one can be available only after fetch of the configuration
// That's a feature of D2WContext. So we use it here
case class RuleFault(rhs: D2WContext, key: String)
case class DrySubstrate(eosAtKeyPath: Option[EOsAtKeyPath] = None, eo: Option[EOFault] = None, fetchSpecification: Option[EOFetchSpecification] = None)
case class WateringScope(fireRule: Option[RuleFault] = None)
//case class EORefsDefinition()
case class EOsAtKeyPath(eo: EO, keyPath: String)
//case class RuleRawResponse(ruleFault: RuleFault, response: WSResponse)

object RuleUtils {

  def metaDataFetched(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext): Boolean  = {
    val ruleContainerOpt = RuleUtils.pageConfigurationRuleResultsForContext(ruleResults,d2wContext)
    ruleContainerOpt match {
      case Some(rulesContainer) => rulesContainer.metaDataFetched
      case None => false
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

  def fireRuleFault(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], ruleFault: RuleFault): Option[RuleResult] = {
    val ruleKey = ruleFault.key
    val ruleRhs = D2WContextUtils.d2wContextByResolvingRuleFaults(ruleResults,ruleFault.rhs)
    RuleUtils.ruleResultForContextAndKey(ruleResults,ruleRhs,ruleKey)
  }




  def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {D2WContextUtils.isD2WContextEquals(r.rhs,rhs) && r.key.equals(key)})
  //def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {r.key.equals(key)})

  def ruleStringValueForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): Option[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleStringValueWithRuleResult(ruleResult)
  }

  def ruleListValueForContextAndKey(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext, key:String): List[String] = {
    val ruleResult = ruleResultForContextAndKey(ruleResults, d2wContext, key)
    ruleListValueWithRuleResult(ruleResult)
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
    val pageConfiguration: String = if (d2wContext.pageConfiguration.isDefined) d2wContext.pageConfiguration.get.right.get else PageConfiguration.NoPageConfiguration
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
  def ruleListValueWithRuleResult(ruleResultOpt: Option[RuleResult]) = {
    ruleResultOpt match {
      case Some(ruleResult) => ruleResult.value.stringsV
      case _ => List()
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


  def convertD2WContextToFullFledged(d2wContext: D2WContext): D2WContextFullFledged = {
    log.debug("D2WContextUtils.convertD2WContextToFullFledged : " + d2wContext.pageConfiguration)
    if(d2wContext.pageConfiguration.isDefined) {
      log.debug("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get " + d2wContext.pageConfiguration.get)
      log.debug("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get.right " + d2wContext.pageConfiguration.get.right)
      log.debug("D2WContextUtils.convertD2WContextToFullFledged : d2wContext.pageConfiguration.get.right.get " + d2wContext.pageConfiguration.get.right.get)

    }

    D2WContextFullFledged(
      d2wContext.entityName,
      d2wContext.task,
      d2wContext.propertyKey,
      if(d2wContext.pageConfiguration.isDefined) Some(d2wContext.pageConfiguration.get.right.get) else None
    )
  }


  def convertD2WContextToFullFledgedByResolvingRuleFaults(ruleResults: Map[String, Map[String, Map[String, PageConfigurationRuleResults]]], d2wContext: D2WContext): D2WContextFullFledged = {
    val resolvedD2WContext = d2wContextByResolvingRuleFaults(ruleResults,d2wContext)
    D2WContextUtils.convertD2WContextToFullFledged(resolvedD2WContext)
  }


  def d2wContextByResolvingRuleFaults(ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]], d2wContext: D2WContext): D2WContext = {
    d2wContext.pageConfiguration match {
      case Some(pc) =>
        pc match {
          case Right(pageConf) =>
            d2wContext
          case Left(pageConfFromRule) =>
            val ruleResultOpt = RuleUtils.fireRuleFault(ruleResults, pageConfFromRule)
            ruleResultOpt match {
              case Some(ruleResult) =>
                val pageConfiguration = ruleResult.value.stringV.get
                val tmpContext = d2wContext.copy(pageConfiguration = Some(Right(pageConfiguration)))
                tmpContext
              case None =>
                // Remove the page configuration is the best thing we can do ... (?)
                val tmpContext = d2wContext.copy(pageConfiguration = None)
                tmpContext
            }
        }
      case None => d2wContext
    }

  }

  def convertFullFledgedToD2WContext(d2wContext: D2WContextFullFledged) = D2WContext(
    d2wContext.entityName,
    d2wContext.task,
    None,
    None,
    Map(),
    None,
    d2wContext.propertyKey,
    if(d2wContext.pageConfiguration.isDefined) Some(Right(d2wContext.pageConfiguration.get)) else None
  )


  def isD2WContextEquals(a: D2WContextFullFledged, b: D2WContext): Boolean  = {
    if (!a.entityName.equals(b.entityName)) return false
    if (!a.task.equals(b.task)) return false
    if (!a.propertyKey.equals(b.propertyKey)) return false
    if (b.pageConfiguration.isDefined && b.pageConfiguration.get.isLeft) return false
    val comparableValue = if (b.pageConfiguration.isDefined) Some(b.pageConfiguration.get.right.get) else None
    if (!a.pageConfiguration.equals(comparableValue)) return false
    return true
  }

}

case class SetMetaDataWithActions(d2WContext: D2WContext, actions: List[D2WAction], metaData: EntityMetaData) extends Action

case class SetRuleResults(ruleResults: List[RuleResult], d2wContext: D2WContext, actions: List[D2WAction]) extends Action
case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: EOEntity, task: String) extends Action
case class SetupQueryPageForEntity(entityName: String) extends Action

case object SwithDebugMode extends Action
case object FetchShowD2WDebugButton extends Action
case class SetDebugConfiguration(debugConf: DebugConf) extends Action

/*      Menus(
        List(
          MainMenu(1, "MCU",
            List(
              Menu(2, "Nagra MCU", "DTEChipset"),
              Menu(3, "EMI", "DTEEMI"),
              Menu(4, "Chipset Security Type", "ChipsetSecurityType")
            )
          )
        ),
        D2WContext("DTEEMI", "query", null)
      ),
*/


object AppModel {
  val bootingModel = AppModel(
    MegaContent(
      false,
      DebugConfiguration(),
      Empty,
      Empty,
      Map(),
      //EditEOFault(Empty,0),
      EOCache(Map(),Map()), //Map.empty[String, EOValue],Map.empty[String, EOValue],
      None
    )
  )

}

object EOCacheUtils {

  // Returns None if nothing registered in the cache for that entityName
  def objectsForEntityNamed(eos: Map[String, Map[Int,EO]], entityName: String): Option[List[EO]] = if (eos.contains(entityName)) {
    val entityEOs = eos(entityName).values.toList
    Some(entityEOs)
  } else None

  def objectForEntityNamedAndPk(eos: Map[String, Map[Int,EO]], entityName: String, pk: Int): Option[EO] =
    if (eos.contains(entityName)) {
      val entityEOById = eos(entityName)

      if (entityEOById.contains(pk)) Some(entityEOById(pk)) else None
    } else None

  def objectsWithFetchSpecification(eos: Map[String, Map[Int,EO]],fs: EOFetchSpecification) : Option[List[EO]] = {
    val entityNameEOs = objectsForEntityNamed(eos,EOFetchSpecification.entityName(fs))
    log.debug("EOCacheUtils.objectsWithFetchSpecification : " + entityNameEOs)
    entityNameEOs match {
      case Some(eos) =>
        Some(EOFetchSpecification.objectsWithFetchSpecification(eos,fs))
      case _ => None
    }
  }

  def outOfCacheEOsUsingPkFromEOs(cache: MegaContent, entityName: String, eos: List[EO]): List[EO] = {
    eos.map(eo => outOfCacheEOUsingPkFromD2WContextEO(cache,entityName,eo)).flatten
  }

  def objectsWithFetchSpecification(cache: EOCache,fetchSpecification: EOFetchSpecification): List[EO] = {
    val eosOpt = objectsWithFetchSpecification(cache.eos,fetchSpecification)
    val insertedEOsOpt = objectsWithFetchSpecification(cache.insertedEOs,fetchSpecification)

    eosOpt match {
      case Some(eos) =>
        insertedEOsOpt match {
          case Some(insertedEOs) =>
            // TODO sorting
            eos ++ insertedEOs
          case _ => eos
        }
      case _ => List.empty[EO]
    }

  }


  def outOfCacheEOUsingPkFromD2WContextEO(cache: MegaContent, entityName: String, eo: EO): Option[EO] = {
    outOfCacheEOUsingPk(cache, entityName, eo.pk)
  }

  def outOfCacheEOUsingPks(cache: MegaContent, entityName: String, pks: Seq[Int]): Seq[EO] = {
    pks.map(pk => outOfCacheEOUsingPk(cache, entityName, pk)).flatten
  }

  def outOfCacheEOUsingPk(cache: MegaContent, entityName: String, pk: Int): Option[EO] = {
    if (pk < 0) {
      val memCache = cache.cache.insertedEOs
      log.debug("Out of cache " + pk)
      log.debug("Cache " + memCache)
      log.debug("e name " + entityName)
      EOCacheUtils.objectForEntityNamedAndPk(memCache, entityName, pk)
    } else {
      val dbCache = cache.cache.eos
      log.debug("DB Cache " + dbCache + " entityName " + entityName + " pk " + pk)
      EOCacheUtils.objectForEntityNamedAndPk(dbCache, entityName, pk)
    }
  }
}


object FireRuleConverter {
  def toRuleFault(fireRule: FireRule) = RuleFault(fireRule.rhs,fireRule.key)
}
