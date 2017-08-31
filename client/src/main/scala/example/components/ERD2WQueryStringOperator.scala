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
import d2spa.shared.{ StringValue, EOKeyValueQualifier, QueryProperty}

object ERD2WQueryStringOperator  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], property: QueryProperty, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      <.div(
        <.input(^.id := "description", ^.value := p.property.value.value,
          ^.placeholder := "write description", ^.onChange ==> { e: ReactEventI => p.proxy.dispatchCB(UpdateQueryProperty(entity,p.property,StringValue(e.target.value)))} )
      )
    }
  }

  private val component = ReactComponentB[Props]("ERD2WQueryStringOperator")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: QueryProperty, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))
}
