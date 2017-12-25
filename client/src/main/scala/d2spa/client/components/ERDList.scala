package d2spa.client.components

import d2spa.client.HydrateProperty
import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.shared.{EO, PropertyMetaInfo, RuleKeys, ValueType}
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
import d2spa.client.{MegaContent, UpdateEOValueForProperty}

object ERDList {

  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)

  class Backend($ : BackendScope[Props, Unit]) {


    def render(p: Props) = {
      val eo = p.eo
      val propertyKey = p.property.d2wContext.propertyKey
      //val eoValue = eo.values(p.property.d2wContext.propertyKey)
      //val size = eoValue.eosV.size
      val size = 1
      <.div(size + "Projects " + propertyKey
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    //.componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,eo, proxy))

}
