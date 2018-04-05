package d2spa.shared

import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}


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

//case class DateValue(value: java.util.Date) extends EOValue

case class EO(entity: EOEntity, values: Map[String,EOValue], memID: Option[Int] = None, validationError: Option[String] = None)


object EO extends MaterializePicklerFallback {
  import boopickle.Default._

  implicit val eoValuePickler = compositePickler[EOValue]
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]

  eoValuePickler.addConcreteType[StringValue].addConcreteType[IntValue].addConcreteType[ObjectValue].addConcreteType[ObjectsValue]

  implicit val qualifierPickler = compositePickler[EOQualifier]
  qualifierPickler.addConcreteType[EOAndQualifier].addConcreteType[EOOrQualifier].addConcreteType[EONotQualifier].addConcreteType[EOKeyValueQualifier]

  implicit val bPickler = generatePickler[EOQualifier]
  def serializer(c: EOFetchSpecification) = Pickle.intoBytes(c)


  //qualifierPickler.join[EOQualifier]

  /*implicit val eoAndQualifierPicker: Pickler[EOAndQualifier] =
    transformPickler[EOAndQualifier, (List[EOQualifier])]((q)  => new EOAndQualifier(q))((q) => (q.qualifiers))
  implicit val eoOrQualifierPicker: Pickler[EOOrQualifier] =
    transformPickler[EOOrQualifier, (List[EOQualifier])]((q)  => new EOOrQualifier(q))((q) => (q.qualifiers))
  implicit val eoNotQualifierPicker: Pickler[EONotQualifier] =
    transformPickler[EONotQualifier, (EOQualifier)]((q)  => new EONotQualifier(q))((q) => (q.qualifier))*/




  // case class EOFetchSpecification (entityName: String, qualifier: Option[EOQualifier] = None, sortOrderings: List[EOSortOrdering] = List())
  //implicit val eoFetchSpecificationPicker: Pickler[EOFetchSpecification] =
  //  transformPickler[EOFetchSpecification, (String, EOQualifier, List[EOSortOrdering])]((fs)  => new EOFetchSpecification(fs._1, if (fs._2 == null) None else Some(fs._2),fs._3))((fs) => (fs.entityName, (if (fs.qualifier.isDefined) fs.qualifier.get else null), fs.sortOrderings))


}


sealed trait EOValue
case class StringValue(value: Option[String]) extends EOValue
case class IntValue(value : Option[Int]) extends EOValue
case class ObjectValue(eo: Option[EO]) extends EOValue
case class ObjectsValue(eos: Seq[EO]) extends EOValue



object EOValue {


  def stringV(value: String) = StringValue(if (value == null) None else Some(value))
  def intV(value: Int) = IntValue(if (value == null) None else Some(value))
  def eoV(value: EO) = ObjectValue(if (value == null) None else Some(value))
  def eosV(value: Seq[EO]) = ObjectsValue(eos = value)


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
    value match {
      case StringValue(value) => if (value.isDefined) value.get else ""
      case IntValue(value) => if (value.isDefined) value.get.toString else ""
      case ObjectValue(eo) => if (eo.isDefined) eo.get.toString else ""
      case _ => ""
    }
  }
  def isDefined(value: EOValue) : Boolean =
    value match {
      case StringValue(value) => value.isDefined
      case IntValue(value) => value.isDefined
      case ObjectValue(eo) => eo.isDefined
      case ObjectsValue(eos) => !eos.isEmpty
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
    existingEO.copy(values = newValues, validationError = refreshedEO.validationError)
  }

  def isNew(eo:EO) = {
    val pk = EOValue.pk(eo)
    (pk.isDefined && pk.get < 0) || pk.isEmpty
    //eo.memID.isDefined
  }

  def globalId(eo:EO) = {
     if (isNew(eo)) {
       eo.memID
     } else pk(eo)
  }

  def pk(eo:EO): Option[Int] = {
    val resultOpt = eo.values.find(value => { value._1.equals(eo.entity.pkAttributeName)})
    resultOpt match {
      case Some((key, eoValue)) =>
        eoValue match {
          case IntValue(pk) =>  pk
          case _ => None
        }
      case _ => None
    }
  }


}

case class EOFetchSpecification (entityName: String, qualifier: Option[EOQualifier] = None, sortOrderings: List[EOSortOrdering] = List())

object EOFetchSpecification {
  def objectsWithFetchSpecification(eos : List[EO], fetchSpecification: EOFetchSpecification): List[EO] = {

    // TODO sort orderings
    fetchSpecification.qualifier match {
      case Some(qualifier) => EOQualifier.filteredEOsWithQualifier(eos,qualifier)
      case _ => eos
    }

  }

}

object EOQualifier {

  def filteredEOsWithQualifier(eos : List[EO], qualifier: EOQualifier) = {
     eos.filter(eo => evaluateWithEO(eo,qualifier))
  }

  def evaluateWithEO(eo: EO, qualifier: EOQualifier): Boolean = {
    qualifier match {
      case EOAndQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        val headQValue = evaluateWithEO(eo,headQ)
        if (!headQValue) {
          false
        } else {
          if (remaining.isEmpty) true else evaluateWithEO(eo, EOAndQualifier(remaining))
        }
      case EOOrQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        if (evaluateWithEO(eo,headQ)) {
          true
        } else {
          if (remaining.isEmpty) false else evaluateWithEO(eo, EOOrQualifier(remaining))
        }
      case EOKeyValueQualifier(key,selector,value) =>
        val eoValue = EOValue.valueForKey(eo,key)

        // TODO check the value
        eoValue.equals(value)
      case EONotQualifier(qualifier) =>
        !evaluateWithEO(eo,qualifier)
    }
  }
}


sealed trait EOQualifier

case class EOAndQualifier(qualifiers : List[EOQualifier]) extends EOQualifier
case class EOOrQualifier(qualifiers : List[EOQualifier]) extends EOQualifier
case class EOKeyValueQualifier(key: String, selector : String, value: EOValue) extends EOQualifier
case class EONotQualifier(qualifier: EOQualifier) extends EOQualifier

case class EOSortOrdering(key: String, selector: String)

case class EOModel(entities: List[EOEntity])
case class EOEntity(name: String, pkAttributeName: String, relationships: List[EORelationship])
case class EORelationship(name: String, destinationEntityName: String)

//case class EORef(entityName: String, id: Int)

case class Menus(menus: List[MainMenu], showDebugButton: Boolean)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: EOEntity)

case class EOFault(entityName : String, pk: Int)
//case class FetchSpecification(entityName: String, qualifier: Option[String] = None)

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





