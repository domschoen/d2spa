package d2spa.client.components

import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, UpdateQueryProperty}
import d2spa.shared.{PropertyMetaInfo, StringValue}

object ERD2WQueryToOneField  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      <.div(
        <.span("where"),
        <.input(^.id := "description", ^.value := p.property.value.value,
          ^.placeholder := "write description", ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entity,p.property,StringValue(e.target.value)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryToOneField")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))
}
