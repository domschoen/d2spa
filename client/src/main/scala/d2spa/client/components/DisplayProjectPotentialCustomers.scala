package d2spa.client.components

import d2spa.client.D2WContext
import d2spa.client.logger.D2SpaLogger
import d2spa.shared.{EO, EOValue, PropertyMetaInfo}
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

import d2spa.client.logger.log

object DisplayProjectPotentialCustomers {

  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
          <.div(
            <.span(^.id := "description", "To be implemented")
          )



    }

  }

  private val component = ScalaComponent.builder[Props]("DisplayProjectPotentialCustomers")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
