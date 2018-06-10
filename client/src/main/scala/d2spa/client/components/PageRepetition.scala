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
import d2spa.client.logger.log
import d2spa.client._
import d2spa.shared._
import diode.data.Ready

object PageRepetition {


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  class Backend($ : BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityNameChanged = !cEntityName.equals(nEntityName)

      val cIsDebugMode = currentProps.proxy.value.appConfiguration.isDebugMode
      val nIsDebugMode = nextProps.proxy.value.appConfiguration.isDebugMode
      val isDebugModeChanged = !cIsDebugMode.equals(nIsDebugMode)



      log.debug("cEntityName " + cEntityName + " nEntityName " + nEntityName)

      val anyChange = entityNameChanged || isDebugModeChanged

      Callback.when(anyChange) {
        mounted(nextProps)
      }
    }



    def mounted(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      log.debug("D2WQueryPage " + entityName)

      val ruleResults = p.proxy.value.ruleResults
      val dataNotFetched = !RuleUtils.metaDataFetched(ruleResults, d2wContext)
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(InitMetaData(entityName)))
    }




    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      log.debug("Render Query page for entity: " + entityName)
      val ruleResultsModel = p.proxy.value.ruleResults

      val ruleContainerOpt = RuleUtils.ruleContainerForContext(ruleResultsModel,d2wContext)

      ruleContainerOpt match {
        case Some(ruleContainer) => {
          log.debug("Render Query ruleContainer: " + ruleContainer)

          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)
          log.debug("Render Query displayPropertyKeys: " + displayPropertyKeys)

          entityDisplayNameOpt match {
            case Some(entityDisplayName) =>
                  <.div(^.className := "repetition d2wPage",
                    <.table(^.className := "query",
                      <.tbody(
                        <.tr(^.className := "attribute customer",
                          <.td(
                            <.table(
                              <.tbody(
                                displayPropertyKeys toTagMod (
                                  propertyKey =>
                                  {
                                    val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                                    <.tr(^.className := "attribute",
                                      <.th(^.className := "propertyName query",
                                        {
                                          val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
                                          displayNameFound match {
                                            case Some(stringValue) => stringValue
                                            case _ => ""
                                          }
                                        }
                                      ),
                                      // Component part
                                      <.td(^.className := "query d2wAttributeValueCell",
                                        D2WComponentInstaller(p.router, propertyD2WContext, null, p.proxy)
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
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps,scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
