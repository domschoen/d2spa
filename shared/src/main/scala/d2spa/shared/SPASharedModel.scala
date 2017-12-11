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
  val displayNameForEntity = "displayNameForEntity"
  val componentName = "componentName"
  val displayPropertyKeys = "displayPropertyKeys"
  val destinationEos = "destinationEos"
}

object ValueType {
  val stringV = "stringV"
  val intV = "intV"
  val eoV = "eoV"
  val eosV = "eosV"
}

case class EOKeyValueQualifier(key: String,value: String)

//case class DateValue(value: java.util.Date) extends EOValue
case class EOValue(typeV: String = "stringV", stringV: Option[String] = None, intV: Option[Int] = None, eoV: Option[EORef] = None, eosV: Seq[EORef] = Seq())

object EOValueUtils {
  def stringV(value: String) = EOValue(stringV = if (value == null) None else Some(value))
  def eoV(value: EORef) = EOValue(typeV=ValueType.eoV, eoV = if (value == null) None else Some(value))
  def eosV(value: Seq[EORef]) = EOValue(typeV=ValueType.eosV, eosV = value)
  def intV(value: Int) = EOValue(intV = Some(value))

  def juiceString(value: EOValue) : String = if (value == null) "" else {
    value.typeV match {
      case ValueType.stringV => if (value.stringV.isDefined) value.stringV.get else ""
      case ValueType.eoV => if (value.eoV.isDefined) value.eoV.get.toString else ""
      case _ => ""
    }
  }
  def isDefined(value: EOValue) : Boolean =
    value.typeV match {
      case ValueType.stringV => value.stringV.isDefined
      case ValueType.intV => value.intV.isDefined
      case ValueType.eoV => value.eoV.isDefined
      case ValueType.eosV => !value.eosV.isEmpty
      case _ => false
    }

}


case class EO(entity: String, values: scala.collection.Map[String,EOValue])
case class EORef(entity: String, displayName: String, id: Int, pkAttributeName: String)

case class Menus(menus: List[MainMenu], d2wContext: D2WContext)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: String)


case class D2WContext(entity: String, task: String, previousTask: String, propertyKey: String)
case class RuleResult(key: String, eovalue: EOValue)


// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
//case class MetaDatas(entityMetaDatas: List[EntityMetaData])

// Property
case class PropertyMetaInfo(d2WContext: D2WContext, ruleKeyValues: List[RuleResult] = List())
//case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: Map[String,RuleResult] )


case class EntityMetaData(entityName: String, displayName: String, queryTask: Task, listTask:Task, inspectTask: Task, editTask: Task)

// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: String, operator: String)

// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
case class Task(displayPropertyKeys: List[PropertyMetaInfo])


/// Important NOTE: Seems that Map is not supported in case classes managed by boopickle