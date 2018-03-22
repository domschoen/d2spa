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
  val propertyType = "propertyType"
}

object ValueType {
  val stringV = "stringV"
  val intV = "intV"
  val eoV = "eoV"
  val eosV = "eosV"
}

case class EOKeyValueQualifier(key: String,value: String)

//case class DateValue(value: java.util.Date) extends EOValue
case class EOValue(typeV: String = "stringV", stringV: Option[String] = None, intV: Option[Int] = None, eoV: Option[EO] = None, eosV: Seq[EO] = Seq())

object EOValueUtils {
  def stringV(value: String) = EOValue(stringV = if (value == null) None else Some(value))
  def eoV(value: EO) = EOValue(typeV=ValueType.eoV, eoV = if (value == null) None else Some(value))
  def eosV(value: Seq[EO]) = EOValue(typeV=ValueType.eosV, eosV = value)
  def intV(value: Int) = EOValue(typeV=ValueType.intV, intV = Some(value))


  //case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])
  def dryEOWith(eomodel: EOModel, entityName: String, pk: Option[Int]) = {
    val entity = EOModelUtils.entityNamed(eomodel,entityName).get
    dryEOWithEntity(entity,pk)
  }
  def memEOWith(eomodel: EOModel, entityName: String, memID: Option[Int]) = {
    val entity = EOModelUtils.entityNamed(eomodel,entityName).get
    EO(entity,Map.empty[String, EOValue],memID)
  }

  def createAndInsertNewObject(insertedEOs: Map[String, Map[Int,EO]], eomodel: EOModel, entityName: String) : (Map[String, Map[Int,EO]],EO) = {
    val entity = EOModelUtils.entityNamed(eomodel,entityName).get
    val insertedEOsForEntityOpt = if (insertedEOs.contains(entityName)) Some(insertedEOs(entityName)) else None
    insertedEOsForEntityOpt match {
      case Some(insertedEOsForEntity) =>
        val newMemID = insertedEOsForEntity.keySet.max + 1
        val newEO = EO(entity,Map.empty[String, EOValue],Some(newMemID))
        val newEntityMap = insertedEOsForEntity + (newMemID -> newEO)
        val newInsertedEOs = insertedEOs + (entityName -> newEntityMap)
        (newInsertedEOs,newEO)
      case None =>
        val newMemID = 1
        val newEO = EO(entity,Map.empty[String, EOValue],Some(newMemID))
        val newEntityMap = Map(newMemID -> newEO)
        val newInsertedEOs = Map(entityName -> newEntityMap)
        (newInsertedEOs,newEO)
    }
  }


  // For creation of objects only ?
  def dryEOWithEntity(entity: EOEntity, pk: Option[Int]) = {
    pk match {
      case Some(pk) =>
        val pkAttributeName = entity.pkAttributeName
        val pkValue = intV(pk)
        val valueMap = Map(pkAttributeName -> pkValue)
        EO(entity, valueMap)
      case None =>
        EO(entity,Map.empty[String, EOValue],None)
    }

  }



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

  def eoValueForKey(eo: EO, key: String) : Option[EO] = {
    valueForKey(eo,key) match {
      case Some(value) => value.eoV
      case None => None
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
    existingEO.copy(values = newValues, validationError = refreshedEO.validationError)
  }

  def isNew(eo:EO) = {
    val pk = EOValueUtils.pk(eo)
    (pk.isDefined && pk.get < 0) || pk.isEmpty
    //eo.memID.isDefined
  }

  def globalId(eo:EO) = {
     if (isNew(eo)) {
       eo.memID
     } else pk(eo)
  }

  def pk(eo:EO) = {
    val eoValue = eo.values.find(value => { value._1.equals(eo.entity.pkAttributeName)})
    eoValue match {
      case Some(myEoValue) =>  myEoValue._2.intV
      case _ => None
    }
  }

}


case class EOModel(entities: List[EOEntity])
case class EOEntity(name: String, pkAttributeName: String, relationships: List[EORelationship])
case class EORelationship(name: String, destinationEntityName: String)

case class EO(entity: EOEntity, values: Map[String,EOValue], memID: Option[Int] = None, validationError: Option[String] = None)
//case class EORef(entityName: String, id: Int)

case class Menus(menus: List[MainMenu], showDebugButton: Boolean)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: EOEntity)

case class EOFault(entityName : String, pk: Int)
case class FetchSpecification(entityName: String, qualifier: Option[String] = None)

//case class PreviousTask(task: String, pk: Option[Int])

case class D2WContextFullFledged(entityName: Option[String],
                      task: Option[String],
                      propertyKey:  Option[String] = None,
                      pageConfiguration: Option[String] = None)

case class RuleResult(rhs: D2WContextFullFledged, key: String, value: RuleValue)
case class RuleValue(stringV: Option[String] = None, stringsV: List[String] = List())

// Kind of cache of entity task d2w rules
// Allows to change the menu without haveing to fetch the display property keys
//case class MetaDatas(entityMetaDatas: List[EntityMetaData])

sealed trait RulesContainer {
  def ruleResults: List[RuleResult]
}

case class TaskFault(entityName: String, taskName: String, override val ruleResults: List[RuleResult] = List()) extends RulesContainer

// Property
case class PropertyMetaInfo(typeV: String = "stringV", name: String, entityName : String, task: String,
                            override val ruleResults: List[RuleResult] = List()) extends RulesContainer
//case class PropertyMetaInfo(d2WContext: D2WContext, value: StringValue, ruleKeyValues: Map[String,RuleResult] )

// A D2W Context of type page (without property)
case class EntityMetaData(d2wContext: D2WContextFullFledged, displayName: String, displayPropertyKeys: List[PropertyMetaInfo])

// A container for value should be used. It would give a way to have not only String
case class QueryValue(key: String,value: String, operator: String)

// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
//case class Task(name: String, displayPropertyKeys: List[PropertyMetaInfo]) extends RulesContainer


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



