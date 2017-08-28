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

sealed abstract class EOValue
case class StringValue(value: String) extends EOValue
case class IntValue(value: Int) extends EOValue
//case class DateValue(value: java.util.Date) extends EOValue

sealed abstract class GenericData
case class EOE(values: String) extends GenericData
case class EO(values: scala.collection.Map[String,StringValue]) extends GenericData
case class EOs(eos: List[EO]) extends GenericData
case object NoData extends GenericData


case class Menus(menus: List[MainMenu], d2wContext: D2WContext)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: String)


case class D2WContext(entity: String, task: String, propertyKey: String)

// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
case class MetaDatas(entityMetaDatas: List[EntityMetaData]) {
  def isEmpty() = {
    entityMetaDatas.isEmpty
  }
}
case class EntityMetaData(entityName: String, displayName: String, queryTask: QueryTask, listTask: ListTask, inspectTask: InspectTask, editTask: EditTask)
//case class EntityMetaData(entityName: String, displayName: String)


// Task
abstract class MetaTask {def displayPropertyKeys: List[MetaProperty]}
case class QueryTask(displayPropertyKeys: List[QueryProperty]) extends MetaTask
//case class ListTask(displayPropertyKeys: List[ListProperty], eos: Seq[EO]) extends MetaTask
case class ListTask(displayPropertyKeys: List[ListProperty]) extends MetaTask
case class InspectTask(displayPropertyKeys: List[InspectProperty]) extends MetaTask
case class EditTask(displayPropertyKeys: List[EditProperty]) extends MetaTask

// Property
abstract class MetaProperty {def key: String; def displayName: String; def componentName: String}
// Query property must evolve to be more like a EOQualifier, currently storing the string to search in value
case class QueryProperty(key: String, displayName: String, componentName: String, value: StringValue) extends MetaProperty
case class ListProperty(key: String, displayName: String, componentName: String) extends MetaProperty
case class InspectProperty(key: String, displayName: String, componentName: String, value: StringValue) extends MetaProperty
case class EditProperty(key: String, displayName: String, componentName: String, value: StringValue) extends MetaProperty

