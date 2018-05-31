package d2spa.client.components


import d2spa.client.EOCacheUtils
import d2spa.client.D2WContext
import d2spa.shared.{EmptyValue, StringValue}
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
import d2spa.client.logger._



object ERD2WEditString  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {


    def render(p: Props) = {
      //log.debug("eo: " + p.eo)
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value, entityName, p.d2wContext.eo.get)
      eoOpt match {
        case Some(eo) =>
          val value = EOValue.stringValueForKey(eo,propertyName)
          <.div(
            <.input(^.id := "description", ^.value := value,
              ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput =>
                p.proxy.dispatchCB(UpdateEOValueForProperty(eo, d2wContext, EOValue.eoValueWithString(e.target.value)))} )
          )
        case None => <.div("No eo out of cache")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditString")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
