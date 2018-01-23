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
  val listConfigurationName = "listConfigurationName"
  val pageConfiguration = "pageConfiguration"
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

  // case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])
  def completeEoWithEo(existingEO: EO, refreshedEO: EO) : EO = {
    val existingValues = existingEO.values
    val refreshedValues = refreshedEO.values
    val newValues = existingValues ++ refreshedValues
    existingEO.copy(values = newValues)
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


case class DrySubstrate(eorefs: Option[EORefsDefinition] = None, eo: Option[EO] = None, fetchSpecification: Option[FetchSpecification] = None)
case class WateringScope(fireRule: Option[RuleFault] = None)
case class EORefsDefinition(eosAtKeyPath: Option[EOsAtKeyPath])
case class EOsAtKeyPath(eo: EO, keyPath: String)
case class FetchSpecification(entityName: String, qualifier: Option[String] = None)

case class RuleFault(rhs: D2WContextFullFledged, key: String)

case class D2WContextFullFledged(entityName: Option[String],
                      task: Option[String],
                      previousTask: Option[String] = None,
                      propertyKey:  Option[String] = None,
                      pageConfiguration: Option[String] = None)
case class D2WContext(entityName: Option[String],
                      task: Option[String],
                      previousTask: Option[String] = None,
                      propertyKey:  Option[String] = None,
                      pageConfiguration: Option[Either[RuleFault,String]] = None)
case class RuleResult(rhs: D2WContextFullFledged, key: String, value: RuleValue)
case class RuleValue(stringV: Option[String] = None, stringsV: List[String] = List())


// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
//case class MetaDatas(entityMetaDatas: List[EntityMetaData])

sealed trait RulesContainer {
  def ruleResults: List[RuleResult]
}

// Property
case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
//case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: Map[String,RuleResult] )


case class EntityMetaData(entity: EOEntity, displayName: String, queryTask: Task, listTask:Task, inspectTask: Task, editTask: Task)

// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: String, operator: String)

// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
case class Task(displayPropertyKeys: List[PropertyMetaInfo], override val ruleResults: List[RuleResult] = List()) extends RulesContainer


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


object EntityMetaDataUtils {

  def taskWithTaskName(entityMetaData: EntityMetaData, taskName: String) = {
    taskName match {
      case TaskDefine.edit => entityMetaData.editTask
      case TaskDefine.inspect => entityMetaData.inspectTask
      case TaskDefine.list => entityMetaData.listTask
      case TaskDefine.query => entityMetaData.queryTask
      case _ =>  entityMetaData.queryTask
    }
  }

}


object RuleUtils {
  def convertD2WContextToFullFledged(d2wContext: D2WContext) = D2WContextFullFledged(
    d2wContext.entityName,
    d2wContext.task,
    d2wContext.previousTask,
    d2wContext.propertyKey,
    if(d2wContext.pageConfiguration.isDefined) Some(d2wContext.pageConfiguration.get.right.get) else None
  )
  def convertFullFledgedToD2WContext(d2wContext: D2WContextFullFledged) = D2WContext(
    d2wContext.entityName,
    d2wContext.task,
    d2wContext.previousTask,
    d2wContext.propertyKey,
    if(d2wContext.pageConfiguration.isDefined) Some(Right(d2wContext.pageConfiguration.get)) else None
  )


  def isD2WContextEquals(a: D2WContextFullFledged, b: D2WContext): Boolean  = {
    if (!a.entityName.equals(b.entityName)) return false
    if (!a.task.equals(b.task)) return false
    if (!a.propertyKey.equals(b.propertyKey)) return false
    if (b.pageConfiguration.isDefined && b.pageConfiguration.get.isLeft) return false
    val comparableValue = if (b.pageConfiguration.isDefined) Some(b.pageConfiguration.get.right.get) else None
    if (!a.pageConfiguration.equals(comparableValue)) return false
    return true
  }


  def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {isD2WContextEquals(r.rhs,rhs) && r.key.equals(key)})
  //def ruleResultForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {r.key.equals(key)})

  def ruleStringValueForContextAndKey(property: PropertyMetaInfo, d2wContext: D2WContext, key:String) = {
    println("Looking for "  + d2wContext)
    val result = ruleResultForContextAndKey(property.ruleResults, d2wContext, key)
    if (result.isDefined) Some(result.get.value.stringV) else None
    //if (result.isDefined) Some(result.get.value) else None
  }
  def existsRuleResultForContextAndKey(property: PropertyMetaInfo, d2wContext: D2WContext, key:String) = ruleResultForContextAndKey(property.ruleResults, d2wContext, key).isDefined


}
