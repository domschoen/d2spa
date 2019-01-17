package d2spa.client.components

import d2spa.client.PageContext
import d2spa.shared.{EO, EOValue}
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage


object DisplayBoolean {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val eoOpt = pageContext.eo

      eoOpt match {
        case Some(eo) =>
          val entityName = d2wContext.entityName.get
          val propertyName = d2wContext.propertyKey.get
          if (eo.values.contains(propertyName)) {

            // We expect a value for that property. Either:
            // StringValue
            // EmptyValue
            val eoValue = EOValue.valueForKey(eo,propertyName).get
            val value = EOValue.juiceBoolean(eoValue)
            <.div(
              (<.i(^.className := "glyphicon glyphicon-ok")).when(value)
            )
          } else {
            <.div(
              <.span("No data found")
            )
          }

        case None =>
          <.div(
            <.span("No eo found")
          )

      }
    }
  }

  private val component = ScalaComponent.builder[Props]("Display Boolean")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
