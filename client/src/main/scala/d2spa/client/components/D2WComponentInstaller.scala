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
import japgolly.scalajs.react.component.Scala




object D2WComponentInstaller  {
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      //log.finest("D2WComponentInstaller | Render with d2wContext: " + d2wContext)
      //log.finest("D2WComponentInstaller | Render")

      //log.finest("Render D2WComponentInstaller " + p.proxy.value.isDebugMode)
      <.div({
        val propertyName = d2wContext.propertyKey.get
        val ruleResults = p.proxy.value.ruleResults

        val componentNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, RuleKeys.componentName)
        log.finest("Render D2WComponentInstaller | D2WContext " + pageContext)
        log.finest("Render D2WComponentInstaller | component name " + componentNameFound)
        componentNameFound match {
          case Some(componentName) => {
            val displayedComponentName = if (p.proxy.value.appConfiguration.isDebugMode) propertyName + " - " + componentName else ""
            componentName match {
              case "ERD2WEditToOneRelationship" => <.span("Component not found: " + componentName) //<.span(ERD2WEditToOneRelationship(p.router, pageContext, p.proxy), displayedComponentName)
              case "DisplayToOne" => <.span(ERD2WDisplayToOne(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WDisplayToOne" => <.span(ERD2WDisplayToOne(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WEditString" => <.span(ERD2WEditString(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WEditNumber" => <.span(ERD2WEditNumber(p.router, pageContext, p.proxy), displayedComponentName)
              case "D2WDisplayNumber" => <.span(D2WDisplayNumber(p.router, pageContext, p.proxy), displayedComponentName)

              case "ERD2WDisplayString" => <.span(ERD2WDisplayString(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WDisplayStringWithLineBreaks" => <.span(ERD2WDisplayString(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WDisplayDateOrNull" => <.span(ERD2WDisplayString(p.router, pageContext, p.proxy), displayedComponentName)
              case "NVD2WDisplayFixedFontString" => <.span(ERD2WDisplayString(p.router, pageContext, p.proxy), displayedComponentName)
              case "ERDList" => <.span(ERDList(p.router, pageContext, p.proxy), displayedComponentName)
              case "DisplayBoolean" => <.span(DisplayBoolean(p.router, pageContext, p.proxy), displayedComponentName)
              case "QueryNameOrAliases" => <.span(ERD2WQueryStringOperator (p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WQueryStringOperator" => <.span(ERD2WQueryStringOperator (p.router, pageContext, p.proxy), displayedComponentName)
              case "ERD2WQueryToOneField" => <.span(ERD2WQueryToOneField (p.router, pageContext, p.proxy), displayedComponentName)
              case "NVQueryBoolean" => <.span(NVQueryBoolean (p.router, pageContext, p.proxy), displayedComponentName)

              // Custom Component
              case "DisplayProjectPotentialCustomers" => <.span(DisplayProjectPotentialCustomers(p.router, pageContext, p.proxy), displayedComponentName)

              // application specific components
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

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]): Scala.Unmounted[D2WComponentInstaller.Props, Unit, D2WComponentInstaller.Backend] = component(Props(ctl, d2wContext, proxy))
}
