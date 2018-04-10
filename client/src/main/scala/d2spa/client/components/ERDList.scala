package d2spa.client.components

import d2spa.client.{FireRule, _}
import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.client.logger.log
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.{TaskAppPage}
import d2spa.client.MegaContent
import d2spa.client.UpdateQueryProperty
import d2spa.shared.{PropertyMetaInfo, EOValue}
import d2spa.shared.TaskDefine
import d2spa.client.{MegaContent, UpdateEOValueForProperty}

object ERDList {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDList ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDList convert EORef into EOs
  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value, entityName, p.d2wContext.eo.get)

      eoOpt match {
        case Some(eo) =>
          val propertyName = d2wContext.propertyKey.get
          val eomodel = p.proxy.value.eomodel.get
          val entity = EOModelUtils.entityNamed(eomodel,entityName).get
          val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
          val destinationEntityName = destinationEntity.name

          val eoValueOpt = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None

          val size = eoValueOpt match {
            // TO Restire case Some(ObjectsValue(eos)) => eos.size
            case _ => 0
          }
          //val size = 1
          val embeddedListD2WContext = D2WContext(entityName = Some(destinationEntityName), task = Some(TaskDefine.list), dataRep = Some(DataRep(eosAtKeyPath = Some(EOsAtKeyPath(eo,propertyName)))))
          <.div(NVListComponent(p.router,embeddedListD2WContext,true, p.proxy))

          //<.div(size + " " + destinationEntityName + " " + propertyName)
        case None => <.div("")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    //.componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
