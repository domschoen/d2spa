package d2spa.shared

import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.shared.WebSocketMessages._



/*case class CustomerContainer(
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
                   ) extends EOContaining*/


/*object Test15 extends MaterializePicklerFallback {
  import boopickle.Default._

  implicit val eoContainingPickler = compositePickler[EOContaining]

  implicit val eoContainerPicker: Pickler[EOContainer] = generatePickler[EOContainer]
  implicit val customerContainerPicker: Pickler[CustomerContainer] = generatePickler[CustomerContainer]
  implicit val countryContainerPicker: Pickler[CountryContainer] = generatePickler[CountryContainer]

  eoContainingPickler.addConcreteType[EOContainer]
    .addConcreteType[CustomerContainer]
    .addConcreteType[CountryContainer]


  def serializer(c: EOContaining) = Pickle.intoBytes(c)
}*/


