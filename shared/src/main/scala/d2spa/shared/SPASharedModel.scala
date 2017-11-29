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

object QueryOperator {
  val Match = "Match"
  val Min = "Min"
  val Max = "Max"
}


object TaskDefine {
  val edit = "edit"
  val inspect = "inspect"
  val list = "list"
  val query = "query"
}

object RuleKeys {
  val keyWhenRelationship = "keyWhenRelationship"
  val displayNameForKeyWhenRelationship = "displayNameForKeyWhenRelationship"
  val displayNameForProperty = "displayNameForProperty"
  val componentName = "componentName"
}



case class EOKeyValueQualifier(key: String,value: String)

sealed abstract class EOValue
case class StringValue(value: String) extends EOValue
case class IntValue(value: Int) extends EOValue
//case class DateValue(value: java.util.Date) extends EOValue

sealed abstract class GenericData
case class EOE(values: String) extends GenericData
case class EO(entity: String, values: scala.collection.Map[String,StringValue]) extends GenericData
case class EOs(eos: List[EO]) extends GenericData
case object NoData extends GenericData


case class Menus(menus: List[MainMenu], d2wContext: D2WContext)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: String)


case class D2WContext(entity: String, task: String, previousTask: String, propertyKey: String)
case class RuleResult(key: String, aValueString: String)


// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
//case class MetaDatas(entityMetaDatas: List[EntityMetaData])

// Property
case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: List[RuleResult])
//case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: Map[String,RuleResult] )


case class EntityMetaData(entityName: String, displayName: String, queryTask: Task, listTask:Task, inspectTask: Task, editTask: Task)

// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: String, operator: String)

// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
case class Task(displayPropertyKeys: List[PropertyMetaInfo])


/// Important NOTE: Seems that Map is not supported in case classes managed by boopickle