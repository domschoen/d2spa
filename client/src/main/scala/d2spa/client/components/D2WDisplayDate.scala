package d2spa.client.components

import d2spa.client.PageContext
import d2spa.shared.{EO, EOValue, Utils}
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.PropertyMetaInfo


object D2WDisplayDate {

  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val eoContainingOpt = pageContext.eo
      eoContainingOpt match {
        case Some(eoContaining) =>
          val eo = eoContaining.eo


          val entityName = d2wContext.entityName.get
          val propertyName = d2wContext.propertyKey.get

          println("D2WDisplayDate " + eo)
          println("D2WDisplayDate entityName " + entityName + " propertyName " + propertyName)

          if (eo.keys.contains(propertyName)) {

            // We expect a value for that property. Either:
            // StringValue
            // EmptyValue
            val eoValue = EOValue.valueForKey(eoContaining,propertyName).get
            val value = EOValue.juiceString(eoValue)
            val date = Utils.juiceStringOrDate(value)
            <.div(
              <.span(^.id := "description", date)
            )
          } else {
            <.div(
              <.span(^.id := "description", "No data found")
            )
          }
        case None =>
          <.div(
            <.span(^.id := "description", "No eo found")
          )

      }
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WDisplayDate")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
