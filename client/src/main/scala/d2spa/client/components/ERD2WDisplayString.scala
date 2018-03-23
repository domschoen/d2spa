package d2spa.client.components


import d2spa.client.D2WContext
import d2spa.shared.{EO, EOValueUtils, PropertyMetaInfo}
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



object ERD2WDisplayString  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val eo = p.eo
      val propertyName = p.property.name
      if (eo.values.contains(propertyName)) {
        val eoValue = eo.values(propertyName)
        val value = EOValueUtils.juiceString(eoValue)
        <.div(
          <.span(^.id := "description", value)
        )
      } else {
        <.div(
          <.span(^.id := "description", "No data found")
        )

      }
    }

  }

  private val component = ScalaComponent.builder[Props]("ERD2WDisplayString")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
