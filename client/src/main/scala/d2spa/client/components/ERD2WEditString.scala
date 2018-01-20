package d2spa.client.components


import d2spa.shared.D2WContext
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
import d2spa.shared.{PropertyMetaInfo, EOValue, EO}
import d2spa.client.{MegaContent, UpdateEOValueForProperty}



object ERD2WEditString  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val eo = p.eo
      val propertyName = p.property.name
      val eoValue = eo.values(propertyName)
      val value = if (eoValue.stringV.isDefined) eoValue.stringV.get else ""
      <.div(
        <.input(^.id := "description", ^.value := value,
          ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput =>
            p.proxy.dispatchCB(UpdateEOValueForProperty(eo, p.d2wContext.entityName.get, p.property, EOValue(stringV = Some(e.target.value))))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditString")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property, eo, proxy))

}
