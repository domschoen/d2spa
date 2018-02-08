package d2spa.client.components

import d2spa.client.EOCacheUtils
import d2spa.shared._
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
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value, p.eo)
      eoOpt match {
        case Some(eo) =>
          val propertyName = p.property.name
          val value = EOValueUtils.stringValueForKey(eo, propertyName)
          <.div(
            <.input(^.id := "description", ^.value := value,
              ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput =>
                p.proxy.dispatchCB(UpdateEOValueForProperty(eo, p.d2wContext.entityName.get, p.property,
                  EOValue(typeV = ValueType.intV, intV = Some(e.target.value.toInt))))
              })
          )
        case None => <.div("")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditNumber")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property, eo, proxy))

}

