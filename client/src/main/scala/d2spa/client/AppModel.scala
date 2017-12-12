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



case class MegaContent(isDebugMode: Boolean, menuModel: Pot[Menus], entityMetaDatas: List[EntityMetaData], eos: Pot[Seq[EO]], eo: Pot[EO], queryValues: List[QueryValue])


// Generic Part



// define actions

case object InitMenu extends Action
case class SetMenus(menus: Menus) extends Action

case class InitMetaData(entity: String) extends Action
case class FetchMetaDataForMenu(entity: String) extends Action

case class SetMetaData(entity: String, metaData: EntityMetaData) extends Action
case class SetMetaDataForMenu(entity: String, metaData: EntityMetaData) extends Action

case class NewEOPage(entity: String) extends Action
case class InstallEditPage(entity: String) extends Action
case class InstallInspectPage(fromTask: String, eo:EO) extends Action
case class InstallQueryPage(entity:String) extends Action
case class SetPreviousPage(entity:String) extends Action

case class EOCreated(eo:EO) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entity: String) extends Action
case class Save(entity: String, eo: EO) extends Action

case class UpdateQueryProperty(entity: String, queryValue: QueryValue) extends Action
case class UpdateEOValueForProperty(entity: String, property: PropertyMetaInfo, value: EOValue) extends Action

case class Search(entity: String) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(entity: String, eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class InspectEO(fromTask: String, eo: EO) extends Action

case class HydrateProperty(property: PropertyMetaInfo, keys: List[String]) extends Action
case class SetRuleResults(property: PropertyMetaInfo, ruleResults: List[RuleResult]) extends Action
case class FireRelationshipData(property: PropertyMetaInfo) extends Action

case class ShowPage(entity: String, task: String) extends Action
case class SetupQueryPageForEntity(entity:String) extends Action

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
      List(),
      Empty,
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



