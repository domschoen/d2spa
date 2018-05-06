package d2spa.shared

import boopickle.Default.{generatePickler, _}
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

case class EO(entity: EOEntity, values: Map[String,EOValue], pk: Int, validationError: Option[String] = None)



object Test8 extends MaterializePicklerFallback {
  import boopickle.Default._
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]

  def serializer(c: ObjectsValue) = Pickle.intoBytes(c)
}


object Test3 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]


  implicit val eoValuePickler = compositePickler[EOValue]

  implicit val stringValuePickler: Pickler[StringValue] = generatePickler[StringValue]
  implicit val intValuePickler: Pickler[IntValue] = generatePickler[IntValue]
  implicit val objectValuePicker: Pickler[ObjectValue] = generatePickler[ObjectValue]
  implicit val objectsValuePicker: Pickler[ObjectsValue] = generatePickler[ObjectsValue]

  eoValuePickler.addConcreteType[StringValue].addConcreteType[IntValue].addConcreteType[ObjectValue].addConcreteType[ObjectsValue]

  def serializer(c: EO) = Pickle.intoBytes(c)
}

object Test5 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]


  implicit val eoValuePickler = compositePickler[EOValue]

  implicit val stringValuePickler: Pickler[StringValue] = generatePickler[StringValue]
  implicit val intValuePickler: Pickler[IntValue] = generatePickler[IntValue]
  implicit val objectValuePicker: Pickler[ObjectValue] = generatePickler[ObjectValue]
  implicit val objectsValuePicker: Pickler[ObjectsValue] = generatePickler[ObjectsValue]

  eoValuePickler.addConcreteType[StringValue].addConcreteType[IntValue].addConcreteType[ObjectValue].addConcreteType[ObjectsValue]

  def serializer(c: EOValue) = Pickle.intoBytes(c)
}




object Test4 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoFetchSpecifactionPicker: Pickler[EOFetchSpecification] = generatePickler[EOFetchSpecification]

  implicit val eoPicker: Pickler[EO] = generatePickler[EO]

  implicit val qualifierPickler = compositePickler[EOQualifier]

  implicit val eoAndQualifierPicker: Pickler[EOAndQualifier] = generatePickler[EOAndQualifier]
  implicit val eoOrQualifierPicker: Pickler[EOOrQualifier] = generatePickler[EOOrQualifier]
  implicit val eoKeyValueQualifierPicker: Pickler[EOKeyValueQualifier] = generatePickler[EOKeyValueQualifier]
  implicit val eoNotQualifierPicker: Pickler[EONotQualifier] = generatePickler[EONotQualifier]

  qualifierPickler.addConcreteType[EOAndQualifier].addConcreteType[EOOrQualifier].addConcreteType[EOKeyValueQualifier].addConcreteType[EONotQualifier]

  def serializer(c: EOQualifier) = Pickle.intoBytes(c)

}




sealed trait EOValue
case class StringValue(value: Option[String]) extends EOValue
case class IntValue(value : Option[Int]) extends EOValue
case class ObjectValue(isSome: Boolean = true, eo: EO = null) extends EOValue
case class ObjectsValue(eos: Seq[Int]) extends EOValue
//case object NoneValue extends EOValue




object EOValue {


  def objectValue(eoOpt: Option[EO]) = {
    eoOpt match {
      case Some(eo) => ObjectValue(eo = eo)
      case None => ObjectValue(false)
    }
  }

  def stringV(value: String) = StringValue(if (value == null) None else Some(value))
  def intV(value: Int) = IntValue(if (value == null) None else Some(value))
  def eoV(value: EO) = if (value == null) ObjectValue(false) else ObjectValue(eo = value)
  def eosV(value: Seq[Int]) = ObjectsValue(eos = value)


  //case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])
  def dryEOWith(eomodel: EOModel, entityName: String, pk: Option[Int]) = {
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    dryEOWithEntity(entity, pk)
  }

  /*def memEOWith(eomodel: EOModel, entityName: String, memID: Option[Int]) = {
    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
    EO(entity, Map.empty[String, EOValue], memID)
  }*/

  def createAndInsertNewObject(insertedEOs: Map[String, Map[Int, EO]], entity: EOEntity): (Map[String, Map[Int, EO]], EO) = {
    val entityName = entity.name
    val insertedEOsForEntityOpt = if (insertedEOs.contains(entityName)) Some(insertedEOs(entityName)) else None
    insertedEOsForEntityOpt match {
      case Some(insertedEOsForEntity) =>
        val newMemID = insertedEOsForEntity.keySet.max + 1
        val newEO = EO(entity, Map.empty[String, EOValue], pk = -newMemID)
        val newEntityMap = insertedEOsForEntity + (newMemID -> newEO)
        val newInsertedEOs = insertedEOs + (entityName -> newEntityMap)
        (newInsertedEOs, newEO)
      case None =>
        val newMemID = 1
        val newEO = EO(entity, Map.empty[String, EOValue], pk = -newMemID)
        val newEntityMap = Map(newMemID -> newEO)
        val newInsertedEOs = Map(entityName -> newEntityMap)
        (newInsertedEOs, newEO)
    }
  }

  def createAndInsertNewObject(insertedEOs: Map[String, Map[Int, EO]], eomodel: EOModel, entityName: String): (Map[String, Map[Int, EO]], EO) = {
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      createAndInsertNewObject(insertedEOs,entity)
  }


  // For creation of objects only ?
  def dryEOWithEntity(entity: EOEntity, pk: Option[Int]) = {
    pk match {
      case Some(pk) =>
        val pkAttributeName = entity.pkAttributeName
        val pkValue = intV(pk)
        val pkInt = pkValue.value.get
        val valueMap = Map(pkAttributeName -> pkValue)
        EO(entity, valueMap, pk = pkInt)
      case None =>
        // TODO review this code, can we have pk = -1 ?
        EO(entity, Map.empty[String, EOValue], pk = -1)
    }

  }


  def juiceString(value: EOValue): String = if (value == null) "" else {
    value match {
      case StringValue(value) => if (value.isDefined) value.get else ""
      case IntValue(value) => if (value.isDefined) value.get.toString else ""
      case ObjectValue(isSome, eo) => if (isSome) eo.toString else ""
      case _ => ""
    }
  }

  def isDefined(value: EOValue): Boolean =
    value match {
      case StringValue(value) => value.isDefined
      case IntValue(value) => value.isDefined
      case ObjectValue(isSome, eo) => isSome
      case ObjectsValue(eos) => !eos.isEmpty
      case _ => false
    }

  def stringValueForKey(eo: EO, key: String) = {
    valueForKey(eo, key) match {
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
  def completeEoWithEo(existingEO: EO, refreshedEO: EO): EO = {
    val existingValues = existingEO.values
    val refreshedValues = refreshedEO.values
    val newValues = existingValues ++ refreshedValues
    existingEO.copy(values = newValues, validationError = refreshedEO.validationError)
  }

  def isNew(eo: EO) = {
    eo.pk < 0
    //val pk = EOValue.pk(eo)
    //(pk.isDefined && pk.get < 0) || pk.isEmpty
    //eo.memID.isDefined
  }


  def pk(eo: EO): Option[Int] = {
    val resultOpt = eo.values.find(value => {
      value._1.equals(eo.entity.pkAttributeName)
    })
    resultOpt match {
      case Some((key, eoValue)) =>
        eoValue match {
          case IntValue(pk) => pk
          case _ => None
        }
      case _ => None
    }
  }


}







sealed trait EOFetchSpecification

case class EOFetchAll(entityName: String, sortOrderings: List[EOSortOrdering] = List()) extends EOFetchSpecification
case class EOQualifiedFetch(entityName: String, qualifier: EOQualifier, sortOrderings: List[EOSortOrdering] = List()) extends EOFetchSpecification


object EOFetchSpecification {
  def objectsWithFetchSpecification(eos: List[EO], fs: EOFetchSpecification): List[EO] = {

    // TODO sort orderings
    fs match {
      case fa: EOFetchAll => eos
      case fq: EOQualifiedFetch =>
        println("filter with Qualifier " + fq.qualifier)
        EOQualifier.filteredEOsWithQualifier(eos, fq.qualifier)
    }
  }

  def entityName(fs: EOFetchSpecification): String = {
    fs match {
      case fa: EOFetchAll => fa.entityName
      case fq: EOQualifiedFetch => fq.entityName
    }

  }

}

object EOQualifier {

  def filteredEOsWithQualifier(eos: List[EO], qualifier: EOQualifier) = {
    eos.filter(eo => evaluateWithEO(eo, qualifier))
  }

  def evaluateWithEO(eo: EO, qualifier: EOQualifier): Boolean = {
    println("evaluateWithEO " + eo + " q " + qualifier)
    qualifier match {
      case EOAndQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        val headQValue = evaluateWithEO(eo, headQ)
        if (!headQValue) {
          false
        } else {
          if (remaining.isEmpty) true else evaluateWithEO(eo, EOAndQualifier(remaining))
        }

      case EOOrQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        if (evaluateWithEO(eo, headQ)) {
          true
        } else {
          if (remaining.isEmpty) false else evaluateWithEO(eo, EOOrQualifier(remaining))
        }
      case EOKeyValueQualifier(key, selector, value) =>
        val eoValueOpt = EOValue.valueForKey(eo, key)

        eoValueOpt match {
          case Some(eoValue) =>
            println("Compare " + eoValue + " with " + value)
            // TODO check the value
            eoValue.equals(value)
          case None =>
            println("Error: try to compare no fetched or non existing value for key " + key + " for entity " + eo.entity.name)
            false
        }


      case EONotQualifier(qualifier) =>
        !evaluateWithEO(eo, qualifier)
    }
  }
}

case class FrontendRequest(text: String)

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





