package example.components

import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import example.css.GlobalStyle

import example.D2SPAMain.{TaskAppPage}
import example.MegaContent
import example.UpdateQueryProperty
import d2spa.shared.{EditInspectProperty, QueryProperty, StringValue}
import example.{MegaContent, UpdateEOValueForProperty}

object ERD2WEditNumber extends EditInspectComponent {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], property: EditInspectProperty, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val eo = p.proxy.value.eo.get
      val eoValue = eo.values(p.property.key)
      <.div(
        <.input(^.id := "description", ^.value := eoValue.value,
          ^.placeholder := "write description", ^.onChange ==> { e: ReactEventI => p.proxy.dispatchCB(UpdateEOValueForProperty(entity,p.property,StringValue(e.target.value)))} )
      )
    }
  }

  private val component = ReactComponentB[Props]("ERD2WEditString")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: EditInspectProperty, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))

}

