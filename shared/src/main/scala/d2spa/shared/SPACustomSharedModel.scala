package d2spa.shared

import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.shared.WebSocketMessages._



sealed trait EOContaining {
  val eo: EO
}
case class EOContainer(eo: EO) extends EOContaining
case class CustomerContainer(
                              trigram: String,
                              name: String,
                              customerType: String,
                              dynamicsAccountID: Option[String],
                              headCountry: CountryContainer,
                              region: String,
                              eo: EO) extends EOContaining



case class CountryContainer (
                     flagImage: String,
                     isoCode: String,
                     name: String,
                     region: String,
                     eo:EO
                   ) extends EOContaining

object EOContaining {

  def updateEOContainingEO(eoc: EOContaining, newEO:EO) = {
    eoc match {
      case eoContainer: EOContainer => eoContainer.copy(eo = newEO)
      case customerContainer: CustomerContainer => customerContainer.copy(eo = newEO)
      case countryContainer: CountryContainer => countryContainer.copy(eo = newEO)
    }
  }

}

object Test15 extends MaterializePicklerFallback {
  import boopickle.Default._

  implicit val eoContainingPickler = compositePickler[EOContaining]

  implicit val eoContainerPicker: Pickler[EOContainer] = generatePickler[EOContainer]
  implicit val customerContainerPicker: Pickler[CustomerContainer] = generatePickler[CustomerContainer]
  implicit val countryContainerPicker: Pickler[CountryContainer] = generatePickler[CountryContainer]

  eoContainingPickler.addConcreteType[EOContainer]
    .addConcreteType[CustomerContainer]
    .addConcreteType[CountryContainer]


  def serializer(c: EOContaining) = Pickle.intoBytes(c)
}


object CustomerEOValue {

  def customContainsValueForKey(eoContaining: EOContaining, key: String): Option[Boolean] = None

  def customContainsValueForKey2(eoContaining: EOContaining, key: String): Option[Boolean] = {
    eoContaining match {
      case customerContainer: CustomerContainer =>
        key match {
          case "preferedName" => Some(true)
          case "dynamicsAccountID" => Some(true)
          case "customerTrigram" => Some(true)
          case _ =>
            Some(EOValue.eoContainsValueForKey(eoContaining.eo, key))
        }
      case _ =>
        None
    }
  }
  def customValueForKey(eoContaining: EOContaining, key: String): Option[Option[EOValue]] = None

  def customValueForKey2(eoContaining: EOContaining, key: String): Option[Option[EOValue]] = {
    eoContaining match {
      case customerContainer: CustomerContainer =>
        key match {
          case "preferedName" =>
            Some(Some(StringValue(customerContainer.name)))
          case "dynamicsAccountID" =>
            customerContainer.dynamicsAccountID match {
              case Some(id) => Some(Some(StringValue(id)))
              case None => Some(Some(EmptyValue))
            }
          case "customerTrigram" =>
            Some(Some(StringValue(customerContainer.trigram)))
          case _ =>
            val eo = eoContaining.eo
            Some(EOValue.eoValueForKey(eo, key))
        }
      case _ =>
        None
    }
  }
}