package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared._
/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)

// , genericPart: GenericData, customPart: CustomData


case class CustomData()



case class MegaContent(isDebugMode: Boolean, menuModel: Pot[Menus], eomodel: Pot[EOModel], entityMetaDatas: List[EntityMetaData], eos: Map[String, Seq[EO]], eo: Pot[EO], queryValues: List[QueryValue])


// Generic Part



// define actions

case object InitMenu extends Action
case class InitMenuAndEO(eo: EO, missingKeys: Set[String]) extends Action
case class SetMenus(menus: Menus) extends Action
case class SetMenusAndEO(menus: Menus, eo: EO, missingKeys: Set[String]) extends Action
case class RefreshEO(eo:EO) extends Action
case object FetchEOModel extends Action
case class SetEOModel(eomodel: EOModel) extends Action

case class FetchObjectsForEntity(entity: EOEntity) extends Action
case class FetchedObjectsForEntity(eos: Seq[EO], entity: EOEntity) extends Action

case class InitMetaData(entity: String) extends Action
case class FetchMetaDataForMenu(task: String, entity: EOEntity) extends Action

case class SetMetaData(metaData: EntityMetaData) extends Action
case class SetMetaDataForMenu(task: String, metaData: EntityMetaData) extends Action

case class NewEOPage(entity: EOEntity) extends Action
case class InstallEditPage(fromTask: String, eo:EO) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case class SetPreviousPage(entity: EOEntity) extends Action

case class EOCreated(eo:EO) extends Action
case class CompleteEO(eo:EO, missingKeys:Set[String]) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entity: EOEntity) extends Action
case class Save(entity: EOEntity, eo: EO) extends Action
case class NewEO(entity: EOEntity, eo: EO) extends Action

case class UpdateQueryProperty(entity: EOEntity, queryValue: QueryValue) extends Action
case class UpdateEOValueForProperty(eo: EO, entity: EOEntity, property: PropertyMetaInfo, value: EOValue) extends Action

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

case class HydrateProperty(property: PropertyMetaInfo, keys: List[String]) extends Action
case class SetRuleResults(property: PropertyMetaInfo, ruleResults: List[RuleResult]) extends Action
case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: EOEntity, task: String) extends Action
case class SetupQueryPageForEntity(entity: EOEntity) extends Action

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
      Empty,
      List()
    )
  )




  def ruleStringValueForKey(property: PropertyMetaInfo, key:String) = {
    val result = property.ruleKeyValues.find(r => {r.key.equals(key)})
    if (result.isDefined) result.head.eovalue.stringV.get else ""
  }
  def rulesContainsKey(property: PropertyMetaInfo, key:String) = property.ruleKeyValues.find(r => {r.key.equals(key)}).isDefined

}



