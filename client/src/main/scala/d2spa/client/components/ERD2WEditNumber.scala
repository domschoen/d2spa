package d2spa.client.components

import d2spa.shared.{EO, PropertyMetaInfo, ValueType}
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

object ERD2WEditNumber {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val eo = p.eo
      val eoValue = eo.values(p.property.d2wContext.propertyKey)
      val value = if (eoValue.intV.isDefined) eoValue.intV.get.toString else ""
      <.div(
        <.input(^.id := "description", ^.value := value,
          ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput => p.proxy.dispatchCB(UpdateEOValueForProperty(eo,entity,p.property,
            EOValue(typeV = ValueType.intV, intV = Some(e.target.value.toInt))))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditNumber")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, property, eo, proxy))

}

