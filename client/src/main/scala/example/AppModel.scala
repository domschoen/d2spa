package example

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



case class MegaContent(menuModel: Pot[Menus], metaDatas: MetaDatas, eos: Pot[Seq[EO]], eo: Pot[EO])


// Generic Part



// define actions

case object InitMenu extends Action
case class SetMenus(menus: Menus) extends Action

case object InitMetaData extends Action
case class SetMetaData(metaData: MetaDatas) extends Action

case class NewEOPage(entity: String) extends Action
case class InstallEditPage(entity: String) extends Action
case class InstallInspectPage(eo:EO) extends Action
case class EOCreated(eo:EO) extends Action

case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entity: String) extends Action
case class Save(entity: String, eo: EO) extends Action

case class UpdateQueryProperty(entity: String, property: QueryProperty, value: StringValue) extends Action
case class UpdateEOValueForProperty(entity: String, property: EditInspectProperty, value: StringValue) extends Action

case class Search(entity: String, qualifiers: List[EOKeyValueQualifier]) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult(entity: String, eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action

case class UpdatedEO(eo: EO) extends Action



case class ShowPage(entity: String, task: String) extends Action


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
      Empty,
      MetaDatas(List()),
      Empty,
      Empty
    )
  )
}



