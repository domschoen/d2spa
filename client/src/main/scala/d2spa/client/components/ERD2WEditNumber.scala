package d2spa.client.components

import d2spa.client.EOCacheUtils
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import d2spa.client.PageContext

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
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val faultOpt = pageContext.eo
      faultOpt match {
        case Some(eo) =>

          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value.cache, entityName, eo)
          eoOpt match {
            case Some(eo) =>
              val value = EOValue.stringValueForKey(eo, propertyName)
              <.div(
                <.input(^.id := "description", ^.value := value,
                  ^.placeholder := "write description", ^.onChange ==> { e: ReactEventFromInput =>
                    p.proxy.dispatchCB(UpdateEOValueForProperty(eo, p.d2wContext, EOValue.eoValueWithInt(e.target.value))
                    )
                  })
              )
            case None => <.div("")
          }
        case None =>
          <.div("No eo in context")
      }

    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditNumber")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}

