package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared.{EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.logger.log
import jdk.nashorn.internal.ir.PropertyKey
/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)


case class MegaContent(showBusyIndicator: Boolean = false,   debugConfiguration: DebugConfiguration, menuModel: Pot[Menus], eomodel: Pot[EOModel], ruleResults: Map[String,Map[String,Map[String,PageConfigurationRuleResults]]],
                       cache: EOCache,
                       previousPage: Option[D2WContext]
                       )



object AppModel {
  val bootingModel = AppModel(
    MegaContent(
      false,
      DebugConfiguration(),
      Empty,
      Empty,
      Map(),
      //EditEOFault(Empty,0),
      EOCache(Map(),Map()), //Map.empty[String, EOValue],Map.empty[String, EOValue],
      None
    )
  )

}

