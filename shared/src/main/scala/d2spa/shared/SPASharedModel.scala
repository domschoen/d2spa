package d2spa.shared

import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.shared.EOValue.eoValueForKey
import d2spa.shared.WebSocketMessages._




object WebSocketMessages {

  // Client ---> Server
  // __________________

  sealed trait WebSocketMsgIn
  final case class ImportCustomers(info: String) extends WebSocketMsgIn
  final case class ImportCountries(info: String) extends WebSocketMsgIn
  final case class StringMsgIn(string: String) extends WebSocketMsgIn
  final case class GetDebugConfiguration(d2wContext: D2WContext) extends WebSocketMsgIn
  final case class FetchEOModel(d2wContext: D2WContext) extends WebSocketMsgIn
  final case class FetchMenus(d2wContext: D2WContext) extends WebSocketMsgIn
  final case class ExecuteRuleRequest(ruleRequest: RuleRequest) extends WebSocketMsgIn
  final case class RuleToFire(rhs: D2WContext, key: String) extends WebSocketMsgIn
  final case class DeleteEOMsgIn(eo: EOContaining) extends WebSocketMsgIn
  //final case class CompleteEO(d2wContext: D2WContextFullFledged, eo: EOFault, missingKeys: Set[String], isMetaDataFetched: Boolean) extends WebSocketMsgIn
  //final case class HydrateEOs(d2wContext: D2WContextFullFledged, pks: Seq[EOPk], missingKeys: Set[String]) extends WebSocketMsgIn
  //final case class HydrateAll(fs: EOFetchAll) extends WebSocketMsgIn
  //final case class Hydrate(fs: EOQualifiedFetch) extends WebSocketMsgIn
  final case class RuleRequestForSearchResult(fs: EOFetchSpecification, eos: Seq[EOContaining], ruleRequest: RuleRequest) extends WebSocketMsgIn

  final case class Hydrate(d2wContext: Option[D2WContext], hydration: Hydration, ruleRequest: Option[RuleRequest]) extends WebSocketMsgIn

  // D2W Context is needed for the fetch of rules
  final case class NewEO(d2wContext: D2WContext, eos: List[EOContaining], ruleRequest: RuleRequest) extends WebSocketMsgIn
  final case class UpdateEO(d2wContext: D2WContext, eos: List[EOContaining], ruleRequest: RuleRequest) extends WebSocketMsgIn
  final case class AppInitMsgIn(ruleRequest: RuleRequest, eoOpt: Option[EOContaining]) extends WebSocketMsgIn

  // Server ---> Client
  // __________________

  sealed trait WebSocketMsgOut

  final case class FetchedEOModel(eomodel: EOModel,d2wContext: D2WContext) extends WebSocketMsgOut
  final case class FetchedMenus(menus: Menus, d2wContext: D2WContext) extends WebSocketMsgOut

  final case class RuleResults(ruleResults: List[RuleResult]) extends WebSocketMsgOut
  final case class RuleRequestResponseMsg(d2wContext: D2WContext, ruleResults: Option[List[RuleResult]]) extends WebSocketMsgOut
  final case class RuleRequestForAppInitResponseMsg(d2wContext: D2WContext, ruleResults: Option[List[RuleResult]], eoOpt: Option[EOContaining]) extends WebSocketMsgOut
  final case class RulesForSearchResultResponseMsgOut(fs: EOFetchSpecification, eos: Seq[EOContaining], ruleResults: Option[List[RuleResult]]) extends WebSocketMsgOut


  final case class FetchedObjectsMsgOut(entityName: String, eos: Seq[EOContaining], ruleResults: Option[List[RuleResult]]) extends WebSocketMsgOut

  final case class CompletedEOMsgOut(d2wContext: Option[D2WContext], hydration: Hydration, eo: List[EOContaining], ruleResults: Option[List[RuleResult]]) extends WebSocketMsgOut
  final case class SavingResponseMsgOut(d2wContext: D2WContext, eos: List[EOContaining], ruleResults: Option[List[RuleResult]]) extends WebSocketMsgOut
  final case class DeletingResponseMsgOut(eo: EOContaining) extends WebSocketMsgOut

  final case class DebugConfMsg(showD2WDebugButton: Boolean, d2wContext: D2WContext) extends WebSocketMsgOut


  //final case class FetchedObjectsForListMsgOut(fs: EOFetchSpecification, eos: Seq[EO]) extends WebSocketMsgOut
}

//class RuleResults(ruleResults: List[RuleResult])



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
  val attributeType = "attributeType"
  val listConfigurationName = "listConfigurationName"
  val inspectConfigurationName = "inspectConfigurationName"
  val listEmbeddedConfigurationName = "listEmbeddedConfigurationName"
  val pageConfiguration = "pageConfiguration"
  val propertyType = "propertyType"
  val destinationEntity = "destinationEntity"
  val isInspectAllowed = "isInspectAllowed"
  val isEditAllowed = "isEditAllowed"
  val isDeleteAllowed = "isDeleteAllowed"
}

object NSSelector {
  val QualifierOperatorEqual = "QualifierOperatorEqual"
  val QualifierOperatorNotEqual = "QualifierOperatorNotEqual"
  val QualifierOperatorLessThan = "QualifierOperatorLessThan"
  val QualifierOperatorGreaterThan = "QualifierOperatorGreaterThan"
  val QualifierOperatorGreaterThanOrEqualTo = "QualifierOperatorGreaterThanOrEqualTo"
  val QualifierOperatorLessThanOrEqualTo = "QualifierOperatorLessThanOrEqualTo"
  val QualifierOperatorContains = "QualifierOperatorContains"
  val QualifierOperatorLike = "QualifierOperatorLike"
  val QualifierOperatorCaseInsensitiveLike = "QualifierOperatorCaseInsensitiveLike"
}

object EOQualifierType {
  val EOAndQualifier = "EOAndQualifier"
  val EOOrQualifier = "EOOrQualifier"
  val EOKeyValueQualifier = "EOKeyValueQualifier"
  val EONotQualifier = "EONotQualifier"
}


// Use cases:
// 1) Display all related eos in an embedded list
//     - embedded list ask for an hydration based on the related eos pks
//     - result of hydration will populate the cache
//     - when rendering the embedded list get the eos out of the cache with pks directly and fearlessly
case class DrySubstrate(
                         eos: Option[EOsFault] = None,
                         eo: Option[EOFault] = None,
                         fetchSpecification: Option[EOFetchSpecification] = None
                       )
case class WateringScope(ruleResult: PotFiredRuleResult)
// case class WateringScope(ruleResult: String)
case class Hydration(drySubstrate: DrySubstrate, wateringScope: WateringScope)
case class PotFiredRuleResult (value: Either[String, RuleResult])
//case class PotFiredRulePropertyKeys (value: Either[FireRule, List[String]])

sealed trait FiringRules
case class FireRule(key: String) extends FiringRules
case class FireRules(propertyKeys: List[String], key: String) extends FiringRules
case class GappedFireRules(fromRuleKey: String, key: String) extends FiringRules

case class RuleRequest(d2wContext: D2WContext, rules: List[FiringRules])

//case class DateValue(value: java.util.Date) extends EOValue

//case class EO(entityName: String, values: Map[String, EOValue] = Map.empty[String, EOValue], pk: EOPk, validationError: Option[String] = None)


// PK is a list of int in the same order as in the eomodel
case class EO(entityName: String, keys: List[String] = List.empty[String], values: List[EOValue] = List.empty[EOValue], pk: EOPk = EOPk(List()), validationError: Option[String] = None, saved: Boolean = true)
//case class EO2(entityName: String, values: List[EOValue2], pk: List[Int], validationError: Option[String] = None)
//case class EO2(entityName: String, values: Map[String,EOValue2] = Map(),  pk: List[Int])
//case class EOValueMap(values: Map[String,String] = Map())

object EO {

  def mapWith(keys: List[String], values: List[EOValue]) = {
    keys.zip(values).toMap
  }

  def updateEOWithMap(eoContaining: EOContaining, keyValues: Map[String, EOValue]) = {
    val eo = eoContaining.eo
    val keys = keyValues.keys.toList
    val values = keyValues.values.toList

    val ueo = eo.copy(keys = keys, values = values)
    EOContaining.updateEOContainingEO(eoContaining,ueo)
  }
}


case class EOPack(eos: List[EO])

object Test3 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]


  implicit val eoValuePickler = compositePickler[EOValue]

  implicit val stringValuePickler: Pickler[StringValue] = generatePickler[StringValue]
  implicit val intValuePickler: Pickler[IntValue] = generatePickler[IntValue]
  implicit val booleanValuePickler: Pickler[BooleanValue] = generatePickler[BooleanValue]
  implicit val objectValuePicker: Pickler[ObjectValue] = generatePickler[ObjectValue]
  implicit val objectsValuePicker: Pickler[ObjectsValue] = generatePickler[ObjectsValue]

  eoValuePickler.addConcreteType[StringValue].addConcreteType[IntValue].addConcreteType[BooleanValue].addConcreteType[ObjectValue].addConcreteType[ObjectsValue]

  def serializer(c: EO) = Pickle.intoBytes(c)
}





object Test8 extends MaterializePicklerFallback {
  import boopickle.Default._
  implicit val bPickler = generatePickler[EO]
  def serializer(c: EOPack) = Pickle.intoBytes(c)
}

// For EOValue and Class extending
object Test5 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]


  implicit val eoValuePickler = compositePickler[EOValue]

  implicit val stringValuePickler: Pickler[StringValue] = generatePickler[StringValue]
  implicit val intValuePickler: Pickler[IntValue] = generatePickler[IntValue]
  implicit val booleanValuePickler: Pickler[BooleanValue] = generatePickler[BooleanValue]
  implicit val objectValuePicker: Pickler[ObjectValue] = generatePickler[ObjectValue]
  implicit val objectsValuePicker: Pickler[ObjectsValue] = generatePickler[ObjectsValue]

  eoValuePickler.addConcreteType[StringValue].addConcreteType[IntValue].addConcreteType[BooleanValue].addConcreteType[ObjectValue].addConcreteType[ObjectsValue]

  def serializer(c: EOValue) = Pickle.intoBytes(c)
}




/*object Test12 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPicker: Pickler[EO2] = generatePickler[EO2]


  implicit val eoValuePickler = compositePickler[EOValue2]

  implicit val stringValuePickler: Pickler[StringValue2] = generatePickler[StringValue2]
  implicit val intValuePickler: Pickler[IntValue2] = generatePickler[IntValue2]
  implicit val booleanValuePickler: Pickler[BooleanValue2] = generatePickler[BooleanValue2]
  implicit val objectValuePicker: Pickler[ObjectValue2] = generatePickler[ObjectValue2]
  implicit val objectsValuePicker: Pickler[ObjectsValue2] = generatePickler[ObjectsValue2]

  eoValuePickler.addConcreteType[StringValue2].addConcreteType[IntValue2].addConcreteType[BooleanValue2].addConcreteType[ObjectValue2].addConcreteType[ObjectsValue2]

  def serializer(c: EOValue2) = Pickle.intoBytes(c)
}*/



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


object Test10 extends MaterializePicklerFallback {
  import boopickle.Default._

  implicit val eoPkPicker: Pickler[EOPk] = generatePickler[EOPk]
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]
  implicit val eoValuePicker: Pickler[EOValue] = generatePickler[EOValue]
  //implicit val eo2Picker: Pickler[EO2] = generatePickler[EO2]
  implicit val eoPackPicker: Pickler[EOPack] = generatePickler[EOPack]



  implicit val webSocketMsgOutPickler = compositePickler[WebSocketMsgOut]

  implicit val debugConfMsgPicker: Pickler[DebugConfMsg] = generatePickler[DebugConfMsg]
  implicit val fetchedEOModelPicker: Pickler[FetchedEOModel] = generatePickler[FetchedEOModel]
  implicit val fetchedMenusPicker: Pickler[FetchedMenus] = generatePickler[FetchedMenus]
  implicit val ruleResultsPicker: Pickler[RuleResults] = generatePickler[RuleResults]
  implicit val ruleRequestResponseMsgPicker: Pickler[RuleRequestResponseMsg] = generatePickler[RuleRequestResponseMsg]
  implicit val ruleRequestForAppInitResponseMsgPicker: Pickler[RuleRequestForAppInitResponseMsg] = generatePickler[RuleRequestForAppInitResponseMsg]
  implicit val completedEOMsgOutPicker: Pickler[CompletedEOMsgOut] = generatePickler[CompletedEOMsgOut]
  implicit val fetchedObjectsPicker: Pickler[FetchedObjectsMsgOut] = generatePickler[FetchedObjectsMsgOut]
  //implicit val fetchedObjectsForListMsgOutPicker: Pickler[FetchedObjectsForListMsgOut] = generatePickler[FetchedObjectsForListMsgOut]
  implicit val savingResponseMsgOutPicker: Pickler[SavingResponseMsgOut] = generatePickler[SavingResponseMsgOut]
  implicit val deletingResponseMsgOutPicker: Pickler[DeletingResponseMsgOut] = generatePickler[DeletingResponseMsgOut]




  webSocketMsgOutPickler.addConcreteType[DebugConfMsg]
    .addConcreteType[FetchedEOModel]
    .addConcreteType[FetchedMenus]
    .addConcreteType[RuleResults]
    .addConcreteType[RuleRequestResponseMsg]
    .addConcreteType[RuleRequestForAppInitResponseMsg]
    .addConcreteType[CompletedEOMsgOut]
    .addConcreteType[FetchedObjectsMsgOut]
    //.addConcreteType[FetchedObjectsForListMsgOut]
    .addConcreteType[SavingResponseMsgOut]
    .addConcreteType[DeletingResponseMsgOut]


  def serializer(c: WebSocketMsgOut) = Pickle.intoBytes(c)
}

object Test11 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val ruleResultPicker: Pickler[RuleResult] = generatePickler[RuleResult]
  implicit val ruleRequestPicker: Pickler[RuleRequest] = generatePickler[RuleRequest]
  //implicit val potFiredRulePropertyKeysPicker: Pickler[PotFiredRulePropertyKeys] = generatePickler[PotFiredRulePropertyKeys]
  implicit val potFiredRuleResultPicker: Pickler[PotFiredRuleResult] = generatePickler[PotFiredRuleResult]
  implicit val wateringScopePicker: Pickler[WateringScope] = generatePickler[WateringScope]
  implicit val ruleValuePicker: Pickler[RuleValue] = generatePickler[RuleValue]



  implicit val firingRulesPickler = compositePickler[FiringRules]

  implicit val fireRulePickler: Pickler[FireRule] = generatePickler[FireRule]
  implicit val fireRulesPickler: Pickler[FireRules] = generatePickler[FireRules]
  implicit val gappedFireRulesPickler: Pickler[GappedFireRules] = generatePickler[GappedFireRules]

  firingRulesPickler.addConcreteType[FireRule].addConcreteType[FireRules].addConcreteType[GappedFireRules]

  def serializer(c: FiringRules) = Pickle.intoBytes(c)
}

object Test12 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoPkPicker: Pickler[EOPk] = generatePickler[EOPk]
  implicit val eoSortOrderingPickler = generatePickler[EOSortOrdering]
  implicit val eoQualifierPickler = generatePickler[EOQualifier]

  implicit val eoFetchSpecificationPickler = compositePickler[EOFetchSpecification]

  implicit val eoFetchAllPickler: Pickler[EOFetchAll] = generatePickler[EOFetchAll]
  implicit val eoQualifiedFetchPickler: Pickler[EOQualifiedFetch] = generatePickler[EOQualifiedFetch]

  eoFetchSpecificationPickler.addConcreteType[EOFetchAll].addConcreteType[EOQualifiedFetch]

  def serializer(c: EOFetchSpecification) = Pickle.intoBytes(c)
}

object Test13 extends MaterializePicklerFallback {

  import boopickle.Default._
  implicit val eoFetchSpecifactionPicker: Pickler[EOFetchSpecification] = generatePickler[EOFetchSpecification]
  implicit val eoPkPicker: Pickler[EOPk] = generatePickler[EOPk]
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]
  implicit val eoFaultPicker: Pickler[EOFault] = generatePickler[EOFault]
  implicit val ruleResultPicker: Pickler[RuleResult] = generatePickler[RuleResult]
  implicit val potFiredRuleResultPicker: Pickler[PotFiredRuleResult] = generatePickler[PotFiredRuleResult]
  implicit val wateringScopePicker: Pickler[WateringScope] = generatePickler[WateringScope]
  implicit val drySubstratePicker: Pickler[DrySubstrate] = generatePickler[DrySubstrate]

  implicit val hydrationPicker: Pickler[Hydration] = generatePickler[Hydration]
}


object Test14 extends MaterializePicklerFallback {
  import boopickle.Default._

  implicit val eoPkPicker: Pickler[EOPk] = generatePickler[EOPk]
  implicit val eoPicker: Pickler[EO] = generatePickler[EO]
  implicit val eoValuePicker: Pickler[EOValue] = generatePickler[EOValue]
  //implicit val eo2Picker: Pickler[EO2] = generatePickler[EO2]
  implicit val ruleRequestPicker: Pickler[RuleRequest] = generatePickler[RuleRequest]
  implicit val hydrationPicker: Pickler[Hydration] = generatePickler[Hydration]



  implicit val webSocketMsgInPickler = compositePickler[WebSocketMsgIn]

  implicit val debugConfMsgPicker: Pickler[StringMsgIn] = generatePickler[StringMsgIn]
  implicit val fetchedEOModelPicker: Pickler[GetDebugConfiguration] = generatePickler[GetDebugConfiguration]
  implicit val fetchedMenusPicker: Pickler[FetchEOModel] = generatePickler[FetchEOModel]
  implicit val ruleResultsPicker: Pickler[FetchMenus] = generatePickler[FetchMenus]
  implicit val ruleRequestResponseMsgPicker: Pickler[ExecuteRuleRequest] = generatePickler[ExecuteRuleRequest]
  implicit val ruleRequestForAppInitResponseMsgPicker: Pickler[RuleToFire] = generatePickler[RuleToFire]
  implicit val completedEOMsgOutPicker: Pickler[DeleteEOMsgIn] = generatePickler[DeleteEOMsgIn]
  implicit val hydratePicker: Pickler[Hydrate] = generatePickler[Hydrate]
  implicit val savingResponseMsgOutPicker: Pickler[NewEO] = generatePickler[NewEO]
  implicit val deletingResponseMsgOutPicker: Pickler[UpdateEO] = generatePickler[UpdateEO]
  implicit val appInitMsgInPicker: Pickler[AppInitMsgIn] = generatePickler[AppInitMsgIn]

  webSocketMsgInPickler.addConcreteType[StringMsgIn]
    .addConcreteType[GetDebugConfiguration]
    .addConcreteType[FetchEOModel]
    .addConcreteType[FetchMenus]
    .addConcreteType[ExecuteRuleRequest]
    .addConcreteType[RuleToFire]
    .addConcreteType[DeleteEOMsgIn]
    .addConcreteType[Hydrate]
    .addConcreteType[NewEO]
    .addConcreteType[UpdateEO]
    .addConcreteType[AppInitMsgIn]


  def serializer(c: WebSocketMsgIn) = Pickle.intoBytes(c)
}



object UrlUtils {
  // TBD Sorting parameter
  // qualifier=product.name='CMS' and parentProductReleases.customer.acronym='ECHO'&sort=composedName|desc
  def qualifiersUrlPart(q: EOQualifier): String = {
    val qualifiersStrings = q match {
      case EOAndQualifier(qualifiers) => qualifiers.map(q => keyValueQualifierUrlPart(q.asInstanceOf[EOKeyValueQualifier]))
      case kvq : EOKeyValueQualifier => List(keyValueQualifierUrlPart(kvq))
      case _ => List()
    }
    return qualifiersStrings.mkString(" and ")
  }

  def keyValueQualifierUrlPart(qualifier: EOKeyValueQualifier): String = {
    val value = qualifier.value
    return value match {
      case StringValue(stringV) => qualifier.key + " caseInsensitiveLike '*" + stringV + "*'"
      case IntValue(i) => "" // TODO
      case BooleanValue(b) => qualifier.key + " = " + (if (b) "1" else "0")
      case ObjectValue(eo) => "" // TODO
      // To Restore case ObjectsValue(eos) => "" // TODO
    }
  }

}

object EOContaining {

  def updateEOContainingEO(eoc: EOContaining, newEO:EO) = {
    eoc match {
      case eoContainer: EOContainer => eoContainer.copy(eo = newEO)
      //case customerContainer: CustomerContainer => customerContainer.copy(eo = newEO)
      //case countryContainer: CountryContainer => countryContainer.copy(eo = newEO)
    }
  }

}

sealed trait EOContaining {
  val eo: EO
  def containsValueForKey(key: String): Boolean
  def valueForKey(key: String): Option[EOValue]
}

case class EOContainer(eo: EO) extends EOContaining {

  def containsValueForKey(key: String): Boolean = {
    EOValue.eoContainsValueForKey(eo,key)
  }
  def valueForKey(key: String): Option[EOValue] = {
    EOValue.eoValueForKey(eo, key)
  }

}


sealed trait EOValue
case class StringValue(value: String) extends EOValue
case class IntValue(value : Int) extends EOValue
case class BooleanValue(value : Boolean) extends EOValue
case class ObjectValue(eo: EOPk) extends EOValue
case class ObjectsValue(eos: List[EOPk]) extends EOValue
case object EmptyValue extends  EOValue
//case object NoneValue extends EOValue

/*sealed trait EOValue2
case class StringValue2(value: String) extends EOValue2
case class IntValue2(value : Int) extends EOValue2
case class BooleanValue2(value : Boolean) extends EOValue2
case class ObjectValue2(eo: EO2) extends EOValue2
case class ObjectsValue2(eos: List[EOPk]) extends EOValue2
case object EmptyValue2 extends  EOValue2*/


case class EOPk(pks: List[Int])

object EOValue {

  def escapeValidationError(eoContaining: EOContaining) : EOContaining = {
    val eo = eoContaining.eo
    val escapedHtml = Utils.escapeHtml(eo.validationError.get)
    eo.copy(validationError = Some(escapedHtml))
    EOContaining.updateEOContainingEO(eoContaining, eo)
  }

  def pkFromValues(eomodel: EOModel, eoContaining: EOContaining) : Option[EOPk] = {
    val eo = eoContaining.eo
    val entity = EOModelUtils.entityNamed(eomodel, eo.entityName).get
    val pkAttributeNames = entity.pkAttributeNames
    val pkOpts = pkAttributeNames.map( pkAttributeName => {
      eoContaining.valueForKey(pkAttributeName) match {
        case Some(IntValue(intV)) => Some(intV)
        case None => None
      }
    })
    val missingValueOpt = pkOpts.find(_.isEmpty)
    if (missingValueOpt.isDefined) {
      None
    } else {
      Some(EOPk(pkOpts.flatten))
    }
  }


  def isNew(pk: EOPk) = pk.pks.find(_ < 0).isDefined

  // When saving an EO, we have to remove any non db attributes
  def purgedEO(eo: EOContaining) = {
    eo
  }




  def size(value: EOValue) = value match {
    case StringValue(s) => s.length
    case IntValue(i) => i.toString.length
    case BooleanValue(value) => 1
    case ObjectValue(eo) => 0
    case ObjectsValue(eos: Seq[EOPk]) => eos.size
    case EmptyValue => 0
  }

  def objectValue(eoContainingOpt: Option[EOContaining]) = {
    eoContainingOpt match {
      case Some(eoContaining) =>
        val eo = eoContaining.eo
        ObjectValue(eo = eo.pk)
      case None => EmptyValue
    }
  }

  def stringV(value: String) = StringValue(value)
  def intV(value: Int) = IntValue(value)
  def eoV(value: EO) =  ObjectValue(eo = value.pk)
  def eosV(value: List[EOPk]) = ObjectsValue(eos = value)


  def eoValueWithString(str: String) = if (str.length == 0) EmptyValue else StringValue(str)
  def eoValueWithInt(str: String) = if (str.length == 0) EmptyValue else IntValue(str.toInt)


  //def createAndInsertNewObject(insertedEOs: Map[String, Map[List[Int], EO]], eomodel: EOModel, entityName: String): (Map[String, Map[List[Int], EO]], EO) = {
  //    val entity = EOModelUtils.entityNamed(eomodel, entityName).get
  //    createAndInsertNewObject(insertedEOs,entity)
  //}


  // For creation of objects only ?
  def dryEOContainerWithEntity(entityName: String, pk: EOPk) = {
    //val pkAttributeNames = entity.pkAttributeNames
    //val valuesMap = pkAttributeNames.zip(pk.map(IntValue(_))).toMap
    val newEO = dryEOWithEntity(entityName, pk)
    EOContainer(newEO)
  }
  def dryEOWithEntity(entityName: String, pk: EOPk) = {
    EO(entityName, List.empty[String], List.empty[EOValue], pk)
  }

  def juiceEOPkString(eoPk: EOPk) = {
    eoPk.pks.mkString("_")
  }



  def juiceString(value: EOValue): String =
    value match {
      case StringValue(value) => if (value == null) "" else value
      case IntValue(value) => value.toString
      case BooleanValue(value) => value.toString
      case ObjectValue(eo) => eo.toString
      case EmptyValue => ""
      case _ => ""
    }


  def juiceInt(v: EOValue): Int =
    v match {
      case StringValue(value) => 0
      case IntValue(value) => value
      case ObjectValue(eo) => 0
      case _ => 0
    }

  def juiceBoolean(v: EOValue): Boolean =
    v match {
      case BooleanValue(value) => value
      case StringValue(value) => value.equals("true")
      case IntValue(value) => value == 1
      case ObjectValue(eo) => false
      case _ => false
    }

  def juiceEO(v: EOValue): Option[EOPk] =
    v match {
      case BooleanValue(value) => None
      case StringValue(value) => None
      case IntValue(value) => None
      case ObjectValue(eo) => Some(eo)
      case _ => None
    }

  def isDefined(value: EOValue): Boolean =
    value match {
      case EmptyValue => false
      case _ => true
    }


  // Return a Key-Value Map with all non empty value
  def definedValues(eo: EO): Map[String,EOValue] = {
    val valueMap = keyValues(eo)
    valueMap filter (v => {
      EOValue.isDefined(v._2)
    })
  }

  def keyValues(eo: EO) = {
    eo.keys.zip(eo.values).toMap
  }


  def stringValueForKey(eoContaining: EOContaining, key: String) = {
    val eo = eoContaining.eo
    if (key.equals("userPresentableDescription")) {
      eo.toString
    } else {
      eoContaining.valueForKey(key) match {
        case Some(value) => juiceString(value)
        case None => ""
      }
    }
  }
  def booleanValueForKey(eoContaining: EOContaining, key: String) = {
    val eo = eoContaining.eo
    eoContaining.valueForKey(key) match {
        case Some(value) => juiceBoolean(value)
        case None => false
      }
  }

  def intValueForKey(eoContaining: EOContaining, key: String) = {
    eoContaining.valueForKey(key) match {
      case Some(value) => juiceInt(value)
      case None => 0
    }
  }


  def eoContainsValueForKey(eo: EO, key: String) = {
    eo.keys.contains(key)
  }





  def eoValueForKey(eo: EO, key: String) = {
    val valueMap = keyValues(eo)
    if (valueMap.contains(key)) {
      Some(valueMap(key))
    } else
      None
  }

  def takeValueForKey(eoContaining: EOContaining, eovalue: EOValue, key: String): EOContaining = {
    val eo = eoContaining.eo
    val valueMap = keyValues(eo)
    val newValueMap = (valueMap - key) + (key -> eovalue)
    takeAllValuesForKeys(eoContaining, newValueMap)
  }

  def takeValuesForKeys(eoContaining: EOContaining, keyValuePairs: Map[String,EOValue]): EOContaining = {
    val eo = eoContaining.eo
    val valueMap = keyValues(eo)
    val updatedMap = valueMap ++ keyValuePairs
    takeAllValuesForKeys(eoContaining, updatedMap)
  }


  // Only if the map has all attribute
  // Not ok for partial update
  def takeAllValuesForKeys(eoContaining: EOContaining, keyValuePairs: Map[String,EOValue]): EOContaining = {
    val eo = eoContaining.eo
    val updatedEO = eo.copy(keys = keyValuePairs.keys.toList, values = keyValuePairs.values.toList)
    EOContaining.updateEOContainingEO(eoContaining, updatedEO)
  }

  // case class EO(entity: EOEntity, values: Map[String,EOValue], validationError: Option[String])
  def completeEoWithEo(existingEOContaining: EOContaining, refreshedEOContaining: EOContaining): EOContaining = {
    val existingEO = existingEOContaining.eo
    val refreshedEO = refreshedEOContaining.eo


    val existingValues = keyValues(existingEO)
    val refreshedValues = keyValues(refreshedEO)
    val newValues = existingValues ++ refreshedValues

    val newEO = existingEO.copy(keys = newValues.keys.toList, values = newValues.values.toList, validationError = refreshedEO.validationError)
    EOContaining.updateEOContainingEO(existingEOContaining,newEO)
  }

  def isNewEO(eoContaining: EOContaining) = {
    val eo = eoContaining.eo

    isNew(eo.pk)
    //val pk = EOValue.pk(eo)
    //(pk.isDefined && pk.get < 0) || pk.isEmpty
    //eo.memID.isDefined
  }

  // For single pk EO
  def pk(eoModel: EOModel, eoContaining: EOContaining): Option[EOPk] = {
    val eo = eoContaining.eo
    val entityName = eo.entityName
    val entityOpt = EOModelUtils.entityNamed(eoModel,entityName)
    entityOpt match {
      case Some(entity) =>
        val pkAttributeNames = entity.pkAttributeNames
        if (eo.keys.contains(pkAttributeNames.head)) {
          Some(EOPk(pkAttributeNames.map(pkAttributeName => EOValue.juiceInt(eoContaining.valueForKey(pkAttributeName).get))))
        } else None
      case None => None
    }
  }

  def refaultEO(eo: EO) = {
    EOFault(eo.entityName, eo.pk)
  }

}







sealed trait EOFetchSpecification

case class EOFetchAll(entityName: String, sortOrderings: List[EOSortOrdering] = List()) extends EOFetchSpecification
case class EOQualifiedFetch(entityName: String, qualifier: EOQualifier, sortOrderings: List[EOSortOrdering] = List()) extends EOFetchSpecification


object EOFetchSpecification {
  def objectsWithFetchSpecification(eos: List[EOContaining], fs: EOFetchSpecification): List[EOContaining] = {

    // TODO sort orderings
    fs match {
      case fa: EOFetchAll => eos
      case fq: EOQualifiedFetch =>
        //println("filter with Qualifier " + fq.qualifier)
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

  def filteredEOsWithQualifier(eos: List[EOContaining], qualifier: EOQualifier) = {
    println("Filter with qualifier " + qualifier)
    eos.filter(eo => evaluateWithEO(eo, qualifier))
  }

  def evaluateWithEO(eoContaining: EOContaining, qualifier: EOQualifier): Boolean = {
    val eo = eoContaining.eo
    //println("evaluateWithEO " + eo + " q " + qualifier)
    qualifier match {
      case EOAndQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        val headQValue = evaluateWithEO(eoContaining, headQ)
        if (!headQValue) {
          false
        } else {
          if (remaining.isEmpty) true else evaluateWithEO(eoContaining, EOAndQualifier(remaining))
        }

      case EOOrQualifier(qualifiers) =>
        val headQ = qualifiers.head
        val remaining = qualifiers.tail
        if (evaluateWithEO(eoContaining, headQ)) {
          true
        } else {
          if (remaining.isEmpty) false else evaluateWithEO(eoContaining, EOOrQualifier(remaining))
        }
      case EOKeyValueQualifier(key, selector, value) =>
        val eoValueOpt = eoContaining.valueForKey(key)

        eoValueOpt match {
          // eo has a value which is not empty
          case Some(eoValue) =>
            //println("Compare " + eoValue + " with " + value)
            // TODO check the value
            eoValue match {
              case StringValue(str) =>
                // Qualifier value hopefully of the same type or empty
                value match {
                  case StringValue(qualStr) =>
                    val lStr = str.toLowerCase
                    val lQualStr = qualStr.toLowerCase
                    lStr.indexOf(lQualStr) >= 0
                  case EmptyValue => false
                  // comparing apple with banana -> more like an error
                  case _ => false
                }
              case EmptyValue => value match {
                case EmptyValue => true
                case _ => false
              }
              case BooleanValue(bv)  =>
                // Qualifier value hopefully of the same type or empty
                value match {
                  case BooleanValue(qualBv) =>
                    bv == qualBv
                  case EmptyValue => false
                  // comparing apple with banana -> more like an error
                  case _ => false
                }

              /// For all other type, we fall back the following default behaviour:
              case _ => eoValue.equals(value)
            }
          // eo has no value defined for that key
          case None =>
            println("Error: try to compare no fetched or non existing value for key " + key + " for entity " + eo.entityName)
            false
        }


      case EONotQualifier(qualifier) =>
        !evaluateWithEO(eoContaining, qualifier)
    }
  }
}

case class FrontendRequest(text: String)
case class FrontendResponse(value: Int)

sealed trait EOQualifier

case class EOAndQualifier(qualifiers : List[EOQualifier]) extends EOQualifier
case class EOOrQualifier(qualifiers : List[EOQualifier]) extends EOQualifier
case class EOKeyValueQualifier(key: String, selector : String, value: EOValue) extends EOQualifier
case class EONotQualifier(qualifier: EOQualifier) extends EOQualifier

case class EOSortOrdering(key: String, selector: String)

object EOSortOrdering {
  val CompareAscending = "CompareAscending"
  val CompareDescending = "CompareDescending"
  val CompareCaseInsensitiveAscending = "CompareCaseInsensitiveAscending"
  val CompareCaseInsensitiveDescending = "CompareCaseInsensitiveDescending"

  def isAscending(sortOrdering: EOSortOrdering) = sortOrdering.selector.equals(EOSortOrdering.CompareAscending)

}

case class EOModel(entities: List[EOEntity])
case class EOEntity(name: String, pkAttributeNames: List[String] = List(), attributes: List[String] = List(), relationships: List[EORelationship] = List())
case class EORelationship(joins: List[EOJoin], name: String, definition: Option[String], isToMany: Boolean, destinationEntityName: String)
case class EOJoin(sourceAttributeName: String, destinationAttributeName: String)

object EOJoin {
  def isReciprocalToJoin(of: EOJoin, otherJoin: EOJoin) = {
    of.sourceAttributeName.equals(otherJoin.destinationAttributeName) && of.destinationAttributeName.equals(otherJoin.sourceAttributeName)
  }

}

object EORelationship {

  def isFlattened(relationship: EORelationship) = {
    relationship.definition match {
      case Some(definition) => true
      case None => false
    }
  }

  def keyPathFirstKey(keyPath: String) = {
    val indexOfDot = keyPath.indexOf(".")
    if (indexOfDot < 0) {
      keyPath
    } else {
      keyPath.substring(0,indexOfDot)
    }
  }

  def relationshipNamed(relationships: List[EORelationship], name: String): Option[EORelationship] = {
    relationships.find(rel => {rel.name.equals(name)})
  }

  //  A --> B
  //        B ---> A ?
  def inverseRelationship(eomodel: EOModel, sourceEntity: EOEntity,  relationship: EORelationship): Option[EORelationship] = {
    val destinationEntity = EOModelUtils.entityNamed(eomodel, relationship.destinationEntityName).get
    destinationEntity.relationships.find(
      rel => {
        EORelationship.isRelationshipReciprocalToRelationship(sourceEntity, relationship, rel)
      })
  }

  def isRelationshipReciprocalToRelationship(sourceEntity: EOEntity, of: EORelationship, otherRelationship: EORelationship): Boolean = {
    if (sourceEntity.name.equals(otherRelationship.destinationEntityName)) {

      val ourJoins = of.joins
      val otherJoins = otherRelationship.joins
      val count = ourJoins.size

      if (count == otherJoins.size) {
        val notReciprocalJoinsOpt = ourJoins.find(join => {
          val notReciprocalJoinOpt = otherJoins.find(otherJoin => !EOJoin.isReciprocalToJoin(join,otherJoin))
          notReciprocalJoinOpt.isDefined
        })
        notReciprocalJoinsOpt.isEmpty
      } else false
    } else false
  }

}

//case class EORef(entityName: String, id: Int)

case class Menus(menus: List[MainMenu]) //, showDebugButton: Boolean)
case class MainMenu(id: Int, title: String,  children: List[Menu])
case class Menu(id:Int, title: String, entity: EOEntity)

case class EOFault(entityName : String, pk: EOPk)
case class EOsFault(entityName : String, pks: Seq[EOPk])

//case class PreviousTask(task: String, pk: Option[Int])


case class PotFiringKey (value: Either[RuleToFire, Option[String]])


case class D2WContext(
                       entityName: Option[String],
                       task: Option[String],
                       propertyKey:  Option[String] = None,
                       pageConfiguration: Option[String] = None,
                       additionalKey: Option[String] = None
                     )


case class RuleResult(rhs: D2WContext, key: String, value: RuleValue)
case class RuleValue(stringV: Option[String] = None, stringsV: List[String] = List())

object RulesUtilities {
  def isEmptyRuleRequest(ruleRequest: RuleRequest) = ruleRequest.rules.isEmpty

  def ruleResultForKey(ruleResults: List[RuleResult], key: String) =
    ruleResults.find(r => {r.key.equals(key)})


  // case class RuleValue(stringV: Option[String] = None, stringsV: List[String] = List())
  def ruleListValueWithRuleResult(ruleResultOpt: Option[RuleResult]) = {
    ruleResultOpt match {
      case Some(ruleResult) =>
        ruleResult.value.stringV match {
          case Some(key) => List(key)
          case None =>
            ruleResult.value.stringsV
        }
      case _ => List()
    }
  }
  def ruleResultFromRuleResultsForContextAndKey(ruleResults: List[RuleResult], rhs: D2WContext, key: String) = ruleResults.find(r => {
    isD2WContextEquals(r.rhs, rhs) && r.key.equals(key)
  })

  def isD2WContextEquals(a: D2WContext, b: D2WContext): Boolean = {
    a.equals(b)
  }

  def missingKeysWith(wateringScope: WateringScope, ruleResultsOpt: Option[List[RuleResult]]) = {
    // 2 cases:
    // 1) the PotFiredRuleResult is a left(string) -> we will find the value in the rule result
    // 2) the PotFiredRuleResult is a left(RuleResult) -> the rule result is the value
    val ruleResultOpt = wateringScope.ruleResult.value match {
      case Left(keyToFire) =>
        ruleResultsOpt match {
          case Some(ruleResults) =>
            RulesUtilities.ruleResultForKey(ruleResults,keyToFire)
          case _ => None
        }
      case Right(ruleResult) => Some(ruleResult)
    }
    ruleResultOpt match {
      case Some(ruleResult) =>
        RulesUtilities.ruleListValueWithRuleResult(Some(ruleResult))
      case None => List()
    }
  }

}


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
case class EntityMetaData(d2wContext: D2WContext, displayName: String, displayPropertyKeys: List[PropertyMetaInfo])


// Task
//case class Task(task: String, displayPropertyKeys: List[PropertyMetaInfo])
//case class Task(name: String, displayPropertyKeys: List[PropertyMetaInfo]) extends RulesContainer


/// Important NOTE: Seems that Map is not supported in case classes managed by boopickle

object EOModelUtils {
  def destinationEntity(eomodel: EOModel, entity: EOEntity, relationshipName: String) = {
    val relationshipOpt = relationshipNamed(eomodel, entity.name, relationshipName)
    relationshipOpt match {
      case Some(relationship) =>
        val destinationEntityName = relationship.destinationEntityName
        entityNamed(eomodel, destinationEntityName)
      case None =>
        None
    }
  }

  def relationshipNamed(eomodel: EOModel, entityName: String, relationshipName: String) = {
    val sourceEntityOpt = entityNamed(eomodel, entityName)
    sourceEntityOpt match {
      case Some(sourceEntity) =>
        sourceEntity.relationships.find(r => r.name.equals(relationshipName))
      case None => None
    }
  }

  def entityNamed(eomodel: EOModel, entityName: String) = eomodel.entities.find(e => e.name.equals(entityName))
}





