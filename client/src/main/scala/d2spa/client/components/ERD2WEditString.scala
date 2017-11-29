package d2spa.client.components


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
import d2spa.shared.{PropertyMetaInfo, StringValue}
import d2spa.client.{MegaContent, UpdateEOValueForProperty}



object ERD2WEditString  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val eo = p.proxy.value.eo.get
      val eoValue = eo.values(p.property.d2WContext.propertyKey)
      <.div(
        <.input(^.id := "description", ^.value := eoValue.value,
          ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput => p.proxy.dispatchCB(UpdateEOValueForProperty(entity,p.property,StringValue(e.target.value)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditString")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))

}
