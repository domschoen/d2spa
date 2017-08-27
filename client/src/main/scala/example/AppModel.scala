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



case class MegaContent(menuModel: Pot[Menus], metaDatas: Pot[MetaDatas], eos: Pot[Seq[EO]])


// Generic Part



// define actions

case object InitMenu extends Action
case class SetMenus(menus: Menus) extends Action

case object InitMetaData extends Action
case class SetMetaData(metaData: MetaDatas) extends Action


case object InitMenuSelection extends Action

case object InitAppModel extends Action

case class SelectMenu(entity: String) extends Action

case class UpdateQueryProperty(entity: String, property: QueryProperty, value: StringValue) extends Action

case class Search(entity: String, qualifiers: List[EOKeyValueQualifier]) extends Action
//case class SearchResult(entity: String, eos: Seq[EO]) extends Action
case class SearchResult( eos: Seq[EO]) extends Action
// similar to:
//case class UpdateAllTodos(todos: Seq[TodoItem]) extends Action





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
      Empty,
      Empty
    )
  )
}



/*
      MetaDatas(
        List(
          EntityMetaData("DTEChipset", "DTE Chipset",
            QueryTask(
              List(
                QueryProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                QueryProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            ListTask(
              List(
                ListProperty("name", "Name","ERD2WQueryStringOperator"),
                ListProperty("operator", "Operator","ERD2WQueryStringOperator")
              ),
              Pot.empty[List[EO]]
            ),
            InspectTask(
              List(
                InspectProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                InspectProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            EditTask(
              List(
                EditProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                EditProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
              )
            )
          ),
          EntityMetaData("DTEEMI", "EMI",
            QueryTask(
              List(
                QueryProperty("name", "Name","ERD2WQueryStringOperator",StringValue("fr"))//,
                //QueryProperty("csad", "CSAD","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            ListTask(
              List(
                ListProperty("name", "Name","ERD2WQueryStringOperator")//,
                //ListProperty("csad", "CSAD","ERD2WQueryStringOperator")
              ),
              Pot.empty[List[EO]]
            ),
            InspectTask(
              List(
                InspectProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                InspectProperty("csad", "CSAD","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            EditTask(
              List(
                EditProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                EditProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
              )
            )
          ),
          EntityMetaData("ChipsetSecurityType", "Chipset Security Type",
            QueryTask(
              List(
                QueryProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                QueryProperty("sec", "sec","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            ListTask(
              List(
                ListProperty("name", "Name","ERD2WQueryStringOperator"),
                ListProperty("sec", "sec","ERD2WQueryStringOperator")
              ),
              Pot.empty[List[EO]]
            ),
            InspectTask(
              List(
                InspectProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                InspectProperty("sec", "sec","ERD2WQueryStringOperator",StringValue("toto"))
              )
            ),
            EditTask(
              List(
                EditProperty("name", "Name","ERD2WQueryStringOperator",StringValue("toto")),
                EditProperty("operator", "Operator","ERD2WQueryStringOperator",StringValue("toto"))
              )
            )
          )
        )
      )

 */