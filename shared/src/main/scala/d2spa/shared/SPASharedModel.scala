package d2spa.shared

import boopickle.Default._

sealed trait TodoPriority

case object TodoLow extends TodoPriority

case object TodoNormal extends TodoPriority

case object TodoHigh extends TodoPriority

case class TodoItem(id: String, timeStamp: Int, content: String, priority: TodoPriority, completed: Boolean)

object TodoPriority {
  implicit val todoPriorityPickler: Pickler[TodoPriority] = generatePickler[TodoPriority]
}

case class EOKeyValueQualifier(key: String,value: String)


sealed abstract class GenericData
case class EOE(values: String) extends GenericData
case class EO(values: Map[String,StringValue]) extends GenericData
case class EOs(eos: List[EO]) extends GenericData
case object NoData extends GenericData

sealed abstract class EOValue
case class StringValue(value: String) extends EOValue
case class IntValue(value: Integer) extends EOValue
case class DateValue(value: java.util.Date) extends EOValue



case class Menus(menus: List[MainMenu], d2wContext: D2WContext)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: String)


case class D2WContext(entity: String, task: String, propertyKey: String)

