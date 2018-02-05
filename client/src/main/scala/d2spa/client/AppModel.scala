package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared._
import boopickle.DefaultBasic._

/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)

// , genericPart: GenericData, customPart: CustomData


case class CustomData()



case class MegaContent(isDebugMode: Boolean, menuModel: Pot[Menus], eomodel: Pot[EOModel], entityMetaDatas: List[EntityMetaData],
                       eos: Map[String, Map[Int,EO]],
                       //eo: Pot[EO],
                       queryValues: List[QueryValue])


// Generic Part



// define actions

case object InitClient extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO, rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action
case object FetchEOModel extends Action
case class SetEOModel(eomodel: EOModel) extends Action

case class FetchedObjectsForEntity(eos: Seq[EO], rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

case class InitMetaData(entity: String) extends Action
case class FetchMetaDataForMenu(task: String, entityName: String) extends Action

case class SetMetaData(metaData: EntityMetaData) extends Action
case class SetMetaDataForMenu(task: String, metaData: EntityMetaData) extends Action

case class NewEOPage(entityName: String) extends Action
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

case class InspectEO(fromTask: String, eo: EO) extends Action
case class DeleteEO(fromTask: String, eo: EO) extends Action
case class DeleteEOFromList(fromTask: String, eo: EO) extends Action
case class EditEO(fromTask: String, eo: EO) extends Action
object ListEOs extends Action
case class DeletedEO(eo:EO) extends Action
case class UpdateEOsForEOOnError(eo:EO) extends Action

trait D2WAction extends diode.Action
case class FireRule(rhs: D2WContext, key: String) extends D2WAction
case class Hydration(drySubstrate: DrySubstrate,  wateringScope: WateringScope) extends D2WAction


case class FireActions(rulesContainer: RulesContainer, actions: List[D2WAction]) extends Action

//implicit val fireActionPickler = CompositePickler[FireAction].

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
      Map(),
      List()
    )
  )

}

object EOCacheUtils {
  def objectsForEntityNamed(eos: Map[String, Map[Int,EO]], entityName: String): Option[List[EO]] = if (eos.contains(entityName)) {
    val entityEOs = eos(entityName).values.toList
    if (entityEOs.isEmpty) None else Some(entityEOs)
  } else None

  def objectForEntityNamedAndPk(eos: Map[String, Map[Int,EO]], entityName: String, pk: Int): Option[EO] = if (eos.contains(entityName)) {
    val entityEOById = eos(entityName)

    if (entityEOById.contains(pk)) Some(entityEOById(pk)) else None
  } else None
}

object FireRuleConverter {
  def toRuleFault(fireRule: FireRule) = RuleFault(RuleUtils.convertD2WContextToFullFledged(fireRule.rhs),fireRule.key)
}
