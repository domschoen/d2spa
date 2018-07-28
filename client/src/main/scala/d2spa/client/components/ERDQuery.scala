package d2spa.client.components

import d2spa.client.logger.log
import d2spa.client.{FireRule, _}
import d2spa.shared._
import diode.data.Ready
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.TaskDefine

object ERDQuery {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDInspect ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDInspect convert EORef into EOs
  class Backend($: BackendScope[Props, Unit]) {

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
      log.debug("PageRepetition | mounted | entityName " + entityName)

      val ruleResults = p.proxy.value.ruleResults
      val dataNotFetched = !RuleUtils.metaDataFetched(ruleResults, d2wContext)
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(InitMetaData(entityName)))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      log.debug("ERDQuery render " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      PageRepetition(p.router,d2wContext, p.proxy)

    }
  }

  private val component = ScalaComponent.builder[Props]("ERDQuery")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
