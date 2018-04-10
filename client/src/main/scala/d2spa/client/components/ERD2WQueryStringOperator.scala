package d2spa.client.components

import d2spa.client.{AppModel, D2WContext, QueryValue}
import d2spa.client.logger.log
import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react.{ReactEventFromInput, _}
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.{TaskAppPage}
import d2spa.client.MegaContent
import d2spa.client.UpdateQueryProperty
import d2spa.client.QueryOperator
import d2spa.shared.{ EOValue, EOKeyValueQualifier, PropertyMetaInfo, RuleKeys}

object ERD2WQueryStringOperator  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val d2wContextOpt = p.proxy.value.previousPage
      d2wContextOpt match {
        case Some(d2wContext) =>
          val propertyD2WContext = p.d2wContext
          val entityName = propertyD2WContext.entityName.get
          val propertyName = propertyD2WContext.propertyKey.get

          val queryValues = d2wContext.queryValues
          log.debug("ERD2WQueryStringOperator " + propertyName + " query values " + queryValues)
          val queryValue = queryValues.find(r => {r.key.equals(propertyName)})
          val value = if (queryValue.isDefined) queryValue.get.value else ""
          <.div(
            <.input(^.id := "description", ^.value := value,
              ^.placeholder := "write description", ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName,
                QueryValue(propertyName,e.target.value, QueryOperator.Match)))} )
          )
        case _ =>
          <.div("no context")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryStringOperator")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
