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




object D2WComponentInstaller  {
  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val eo = p.eo
      val property = p.property

      <.div({
        val componentNameFound = property.ruleKeyValues.find(r => {
          r.key.equals(RuleKeys.componentName)
        })
        componentNameFound match {
          case Some(ruleResult) => {
            val componentNameOpt = ruleResult.eovalue.stringV

            componentNameOpt match {
              case Some(componentName) => {
                componentName match {
                  case "ERD2WEditToOneRelationship" => ERD2WEditToOneRelationship(p.router, property, p.proxy.value.eo.get, p.proxy)
                  case "ERD2WEditString" => ERD2WEditString(p.router, property, eo, p.proxy)
                  case "ERD2WEditNumber" => ERD2WEditNumber(p.router, property, eo, p.proxy)
                  case "D2WDisplayNumber" => D2WDisplayNumber(p.router, property, eo, p.proxy)
                  case "ERD2WDisplayString" => ERD2WDisplayString(p.router, property, eo, p.proxy)
                  case _ => <.span("Component not found: " + componentName)
                }
              }
              case _ => <.span("Rule Result with empty value for property " + property.d2wContext.propertyKey)
            }
          }
          case _ => <.span("No component Rule found for property " + property.d2wContext.propertyKey)
        }
      }
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WComponentInstaller")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, property, eo, proxy))
}
