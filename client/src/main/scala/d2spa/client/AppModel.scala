package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared.{EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.logger.log
/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)

// , genericPart: GenericData, customPart: CustomData


case class CustomData()



case class MegaContent(isDebugMode: Boolean, menuModel: Pot[Menus], eomodel: Pot[EOModel], entityMetaDatas: List[EntityMetaData],
                       cache: EOCache,
                       previousPage: Option[D2WContext]
                       )


case class D2WContextEO(pk: Option[Int] = None, memID : Option[Int] = None)



case class EOCache(eos: Map[String, Map[Int,EO]],
                    insertedEOs: Map[String, Map[Int,EO]])



// define actions

case object InitClient extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class UpdateRefreshEOInCache(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class UpdateEOInCache(eo:EO) extends Action
case object FetchEOModel extends Action
case class SetEOModel(eomodel: EOModel) extends Action

case class FetchedObjectsForEntity(eos: Seq[EO], rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

case class InitMetaData(entity: String) extends Action
case class SetPageForTaskAndEntity(d2wContext: D2WContext) extends Action
case class CreateEO(entityName:String) extends Action

case class SetMetaData(metaData: EntityMetaData) extends Action
case class SetMetaDataForMenu(d2wContext: D2WContext, metaData: EntityMetaData) extends Action

case class NewEOWithEOModel(eomodel: EOModel, entityName: String, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class NewEOWithEntityName(entityName: String, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class NewEOCreated(eo: EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case object SetPreviousPage extends Action
case class RegisterPreviousPage(d2WContext: D2WContext) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entityName: String) extends Action
case class Save(entityName: String, eo: EO) extends Action
case class SaveNewEO(entity: EOEntity, eo: EO) extends Action

case class UpdateQueryProperty(entityName: String, queryValue: QueryValue) extends Action
case class UpdateEOValueForProperty(eo: EO, entityName: String, property: PropertyMetaInfo, value: EOValue) extends Action

case class Search(entity: EOEntity) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(entity: EOEntity, eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class SavedEO(fromTask: String, eo: EO) extends Action
case class DeleteEO(fromTask: String, eo: EO) extends Action
case class DeleteEOFromList(fromTask: String, eo: EO) extends Action
case class EditEO(fromTask: String, eo: EO) extends Action
case class SaveError(eo: EO) extends Action
object ListEOs extends Action
case class DeletedEO(eo:EO) extends Action
case class UpdateEOsForEOOnError(eo:EO) extends Action

trait D2WAction extends diode.Action
case class FireRule(rhs: D2WContext, key: String) extends D2WAction
case class Hydration(drySubstrate: DrySubstrate,  wateringScope: WateringScope) extends D2WAction
case class CreateMemID(entityName: String) extends D2WAction
case class FetchMetaData(entityName: String) extends D2WAction

case class FireActions(rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

//implicit val fireActionPickler = CompositePickler[FireAction].

// The D2WContext will contains also the queryValues
// it should contain everything needed to redisplay a page
case class D2WContext(entityName: Option[String],
                      task: Option[String],
                      previousTask: Option[D2WContext] = None,
                      //pageCounter: Int = 0,
                      eo: Option[D2WContextEO] = None,
                      queryValues: List[QueryValue] = List(),
                      propertyKey:  Option[String] = None,
                      pageConfiguration: Option[Either[RuleFault,String]] = None)

case class RuleFault(rhs: D2WContextFullFledged, key: String)
case class DrySubstrate(eorefs: Option[EORefsDefinition] = None, eo: Option[EOFault] = None, fetchSpecification: Option[FetchSpecification] = None)
case class WateringScope(fireRule: Option[RuleFault] = None)
case class EORefsDefinition(eosAtKeyPath: Option[EOsAtKeyPath])
case class EOsAtKeyPath(eo: EO, keyPath: String)

object RuleUtils {


  def fireRuleFault(ruleFault: RuleFault, rulesContainer: RulesContainer): Option[RuleResult] = {
    val ruleKey = ruleFault.key
    val ruleRhs = ruleFault.rhs
    RuleUtils.ruleResultForContextAndKey(rulesContainer.ruleResults,ruleRhs,ruleKey)
  }

  def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContextFullFledged, key: String) = ruleResults.find(r => {D2WContextUtils.isD2WContextEquals(r.rhs,rhs) && r.key.equals(key)})
  //def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {r.key.equals(key)})

  def ruleStringValueForContextAndKey(property: PropertyMetaInfo, d2wContext: D2WContextFullFledged, key:String) = {
    val result = ruleResultForContextAndKey(property.ruleResults, d2wContext, key)
    result match {
      case Some(ruleResult) => ruleResult.value.stringV
      case _ => None
    }
  }

  def existsRuleResultForContextAndKey(property: PropertyMetaInfo, d2wContext: D2WContextFullFledged, key:String) = ruleResultForContextAndKey(property.ruleResults, d2wContext, key).isDefined


}

object D2WContextUtils {
  def convertD2WContextToFullFledged(d2wContext: D2WContext) = D2WContextFullFledged(
    d2wContext.entityName,
    d2wContext.task,
    d2wContext.propertyKey,
    if(d2wContext.pageConfiguration.isDefined) Some(d2wContext.pageConfiguration.get.right.get) else None
  )
  def convertFullFledgedToD2WContext(d2wContext: D2WContextFullFledged) = D2WContext(
    d2wContext.entityName,
    d2wContext.task,
    None,
    None,
    List(),
    d2wContext.propertyKey,
    if(d2wContext.pageConfiguration.isDefined) Some(Right(d2wContext.pageConfiguration.get)) else None
  )


  def isD2WContextEquals(a: D2WContextFullFledged, b: D2WContextFullFledged): Boolean  = {
    if (!a.entityName.equals(b.entityName)) return false
    if (!a.task.equals(b.task)) return false
    if (!a.propertyKey.equals(b.propertyKey)) return false
    if (b.pageConfiguration.isDefined && b.pageConfiguration.get.isLeft) return false
    val comparableValue = if (b.pageConfiguration.isDefined) Some(b.pageConfiguration.get.right.get) else None
    if (!a.pageConfiguration.equals(comparableValue)) return false
    return true
  }

}

case class SetMetaDataWithActions(taskName: String, actions: List[D2WAction], metaData: EntityMetaData) extends Action

case class SetRuleResults(ruleResults: List[RuleResult], rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: EOEntity, task: String) extends Action
case class SetupQueryPageForEntity(entityName: String) extends Action

case object SwithDebugMode extends Action

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
      Empty,
      Empty,
      List(),
      //EditEOFault(Empty,0),
      EOCache(Map(),Map()), //Map.empty[String, EOValue],Map.empty[String, EOValue],
      None
    )
  )

  def propertyMetaDataWithEntityMetaDatas(entityMetaDatas: List[EntityMetaData], entityName: String, taskName: String, propertyName: String) = {
    val entityMetaData = entityMetaDataFromMegaContentForEntityNamed(entityMetaDatas,entityName).get
    propertyMetaDataWithEntityMetaData(entityMetaData,taskName,propertyName)
  }

  def entityMetaDataFromMegaContentForEntityNamed(entityMetaDatas: List[EntityMetaData], entityName: String): Option[EntityMetaData] =
    entityMetaDatas.find(emd => emd.entity.name.equals(entityName))

  def propertyMetaDataWithEntityMetaData(entityMetaData: EntityMetaData, taskName: String, propertyName: String) = {
    val task = taskName match {
      case TaskDefine.edit => entityMetaData.editTask
      case TaskDefine.list => entityMetaData.listTask
      case TaskDefine.inspect => entityMetaData.inspectTask
      case TaskDefine.query => entityMetaData.queryTask
      case _ => entityMetaData.queryTask
    }
    task.displayPropertyKeys.find(p => p.name.equals(propertyName))
  }
}

object EOCacheUtils {
  def objectsForEntityNamed(eos: Map[String, Map[Int,EO]], entityName: String): Option[List[EO]] = if (eos.contains(entityName)) {
    val entityEOs = eos(entityName).values.toList
    if (entityEOs.isEmpty) None else Some(entityEOs)
  } else None

  def objectForEntityNamedAndPk(eos: Map[String, Map[Int,EO]], entityName: String, pk: Int): Option[EO] =
    if (eos.contains(entityName)) {
      val entityEOById = eos(entityName)

      if (entityEOById.contains(pk)) Some(entityEOById(pk)) else None
    } else None

  def outOfCacheEOUsingPkFromEO(cache: MegaContent, eo: EO): Option[EO] = {
    val entity = eo.entity
    eo.memID match {
      case Some(memID) =>
        val memCache = cache.cache.insertedEOs
        log.debug("Out of cache " + memID)
        log.debug("Cache " + memCache)
        log.debug("e name " + entity.name)
        EOCacheUtils.objectForEntityNamedAndPk(memCache,entity.name,memID)
      case None =>
        val dbCache = cache.cache.eos
        val pkOpt = EOValueUtils.pk(eo)
        pkOpt match {
          case Some(pk) =>
            EOCacheUtils.objectForEntityNamedAndPk(dbCache,entity.name,pk)
          case None => None
        }
    }
  }
}


object FireRuleConverter {
  def toRuleFault(fireRule: FireRule) = RuleFault(D2WContextUtils.convertD2WContextToFullFledged(fireRule.rhs),fireRule.key)
}
