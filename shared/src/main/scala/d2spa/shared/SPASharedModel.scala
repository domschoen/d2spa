package d2spa.shared

import boopickle.Default._


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
  val pkAttributeName = "pkAttributeName"
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
  def intV(value: Int) = EOValue(typeV=ValueType.intV, intV = Some(value))


  def juiceString(value: EOValue) : String = if (value == null) "" else {
    value.typeV match {
      case ValueType.stringV => if (value.stringV.isDefined) value.stringV.get else ""
      case ValueType.intV => if (value.intV.isDefined) value.intV.get.toString else ""
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

  def stringValueForKey(eo: EO, key: String) = {
    valueForKey(eo,key) match {
      case Some(value) => juiceString(value)
      case None => ""
    }
  }

  def valueForKey(eo: EO, key: String) = {
    if (eo.values.contains(key)) {
      Some(eo.values(key))
    } else
      None
  }

  def pk(eo:EO) = {
    val eoValue = eo.values.find(value => { value._1.equals(eo.entity.pkAttributeName)})
    eoValue match {
      case Some(myEoValue) =>
        println("pk " + myEoValue)

        val result = myEoValue._2.intV
        println("pk  result " + result)

        result
      case _ => None
    }
  }

}


case class EOModel(entities: List[EOEntity])
case class EOEntity(name: String, pkAttributeName: String, relationships: List[EORelationship])
case class EORelationship(name: String, destinationEntityName: String)

case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])
case class EORef(entityName: String, id: Int)

case class Menus(menus: List[MainMenu], d2wContext: D2WContext, showDebugButton: Boolean)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: EOEntity)


case class D2WContext(entity: EOEntity, task: String, previousTask: String, propertyKey: String)
case class RuleResult(key: String, eovalue: EOValue)


// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
//case class MetaDatas(entityMetaDatas: List[EntityMetaData])

// Property
case class PropertyMetaInfo(typeV: String = "stringV", d2wContext: D2WContext, ruleKeyValues: List[RuleResult] = List())
//case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: Map[String,RuleResult] )


case class EntityMetaData(entity: EOEntity, displayName: String, queryTask: Task, listTask:Task, inspectTask: Task, editTask: Task)

// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: String, operator: String)

// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
case class Task(displayPropertyKeys: List[PropertyMetaInfo])


/// Important NOTE: Seems that Map is not supported in case classes managed by boopickle

object EOModelUtils {
  def destinationEntity(eomodel: EOModel, entity: EOEntity, relationshipName: String) = {
    val sourceEntity = entityNamed(eomodel,entity.name).get
    val relationship = sourceEntity.relationships.find(r => r.name.equals(relationshipName)).get
    val destinationEntityName = relationship.destinationEntityName
    val destinationEntity = entityNamed(eomodel,destinationEntityName).get
    destinationEntity
  }

  def entityNamed(eomodel: EOModel, entityName: String) = eomodel.entities.find(e => e.name.equals(entityName))
}