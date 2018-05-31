package d2spa.client.components

import d2spa.client._
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
import d2spa.client.components.Bootstrap._
import d2spa.client.components.GlobalStyles

import scalacss.ScalaCssReact._
import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.logger._




object D2WComponentInstaller  {
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      log.debug("D2WComponentInstaller | Render with d2wContext: " + d2wContext)

      val eo = p.eo
      //log.debug("Render D2WComponentInstaller " + p.proxy.value.isDebugMode)
      <.div({
        val propertyName = d2wContext.propertyKey.get
        val ruleResults = p.proxy.value.ruleResults

        val componentNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, RuleKeys.componentName)
        componentNameFound match {
          case Some(componentName) => {
            val displayedComponentName = if (p.proxy.value.debugConfiguration.isDebugMode) componentName else ""
            componentName match {
              case "ERD2WEditToOneRelationship" => <.span(ERD2WEditToOneRelationship(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "ERD2WEditString" => <.span(ERD2WEditString(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "ERD2WEditNumber" => <.span(ERD2WEditNumber(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "D2WDisplayNumber" => <.span(D2WDisplayNumber(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "ERD2WDisplayString" => <.span(ERD2WDisplayString(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "ERDList" => <.span(ERDList(p.router, d2wContext, eo, p.proxy), displayedComponentName)
              case "QueryNameOrAliases" => <.span(ERD2WQueryStringOperator (p.router, d2wContext, p.proxy), displayedComponentName)
              case "ERD2WQueryStringOperator" => <.span(ERD2WQueryStringOperator (p.router, d2wContext, p.proxy), displayedComponentName)
              case "ERD2WQueryToOneField" => <.span(ERD2WQueryToOneField (p.router, d2wContext, p.proxy), displayedComponentName)
              case "NVQueryBoolean" => <.span(NVQueryBoolean (p.router, d2wContext, p.proxy), displayedComponentName)
              case _ => <.span("Component not found: " + componentName)
            }
          }
          case _ => <.span("Rule Result with empty value for property " + propertyName)
        }
      })
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WComponentInstaller")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))
}
