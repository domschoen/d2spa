package d2spa.client.components

import d2spa.client.D2WContext
import d2spa.shared.{EO, EOValue}
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.PropertyMetaInfo


object D2WDisplayNumber {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val eo = p.eo
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      if (eo.values.contains(propertyName)) {

        // We expect a value for that property. Either:
        // StringValue
        // EmptyValue
        val eoValue = eo.values(propertyName)
        val value = EOValue.juiceString(eoValue)
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

  private val component = ScalaComponent.builder[Props]("D2WDisplayNumber")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
