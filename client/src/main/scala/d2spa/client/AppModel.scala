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
                       editEOFault: EditEOFault,
                       cache: EOCache,
                       queryValues: List[QueryValue])

case class EditEOFault(eo: Pot[EO], newCounter : Int)

case class EOCache(eos: Map[String, Map[Int,EO]],
                    insertedEOs: Map[String, Map[Int,EO]])


// Generic Part



// define actions

case object InitClient extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class PutRefreshEOInCache(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case object FetchEOModel extends Action
case class SetEOModel(eomodel: EOModel) extends Action

case class FetchedObjectsForEntity(eos: Seq[EO], rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

case class InitMetaData(entity: String) extends Action
case class SetPageForTaskAndEntity(task: String, entityName: String, pageCounter: Int, pk: Option[Int]) extends Action
case class CreateEO(entityName:String) extends Action

case class SetMetaData(metaData: EntityMetaData) extends Action
case class SetMetaDataForMenu(task: String, pageCounter: Int, metaData: EntityMetaData) extends Action

case class NewEOWithEOModel(eomodel: EOModel, entityName: String, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class NewEOWithEntityName(entityName: String, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case class NewEOCreated(eo: EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case class SetPreviousPage(entity: EOEntity) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entityName: String) extends Action
case class Save(entityName: String, eo: EO) extends Action
case class NewEO(entity: EOEntity, eo: EO) extends Action

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
      EditEOFault(Empty,0),
      EOCache(Map(),Map()), //Map.empty[String, EOValue],Map.empty[String, EOValue]
      List()
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
  def toRuleFault(fireRule: FireRule) = RuleFault(RuleUtils.convertD2WContextToFullFledged(fireRule.rhs),fireRule.key)
}
