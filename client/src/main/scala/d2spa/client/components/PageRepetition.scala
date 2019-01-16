package d2spa.client.components

import d2spa.client.D2WQueryPage.Props
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{Button, CommonStyle}
import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.react.ModelProxy
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.client._
import d2spa.shared._
import diode.data.Ready

object PageRepetition {


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])

  class Backend($ : BackendScope[Props, Unit]) {






    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName,"Render PageRepetition for entity: " + d2wContext)
      D2SpaLogger.logfinest(entityName,"Render PageRepetition for entity | object exists: " + d2wContext.eo.isDefined)
      val ruleResultsModel = p.proxy.value.ruleResults

      val ruleContainerOpt = RuleUtils.ruleContainerForContext(ruleResultsModel,d2wContext)

      ruleContainerOpt match {
        case Some(ruleContainer) => {
          D2SpaLogger.logfinest(entityName,"Render PageRepetition ruleContainer: " + ruleContainer)

          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)
          D2SpaLogger.logfinest(entityName,"Render PageRepetition displayPropertyKeys: " + displayPropertyKeys)
          val taskName = d2wContext.task.get

          entityDisplayNameOpt match {
            case Some(entityDisplayName) =>
                  <.div(^.className := "repetition d2wPage",
                    <.table(^.className := taskName,
                      <.tbody(
                        <.tr(^.className := "attribute customer",
                          <.td(
                            <.table(
                              <.tbody(
                                displayPropertyKeys toTagMod (
                                  propertyKey =>
                                  {
                                    val propertyD2WContext = d2wContext.copy(propertyKey = Some(propertyKey))
                                    val propertyPageContext = pageContext.copy(d2wContext = propertyD2WContext)
                                    <.tr(^.className := "attribute",
                                      <.th(^.className := "propertyName " + taskName,
                                        {
                                          val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
                                          displayNameFound match {
                                            case Some(stringValue) => stringValue
                                            case _ => ""
                                          }
                                        }
                                      ),
                                      // Component part
                                      <.td(^.className := taskName + " d2wAttributeValueCell",
                                        D2WComponentInstaller(p.router, propertyPageContext, p.proxy)
                                      )
                                    )
                                  }
                                  )
                              )
                            )
                          )
                        )
                      )
                    )
              )
            case _ => <.div("no entity display name")
          }
        }
        case _ =>
          <.div("no meta datas")
      }
    }


  }


  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
