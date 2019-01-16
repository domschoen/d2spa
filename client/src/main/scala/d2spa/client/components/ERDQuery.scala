package d2spa.client.components

import d2spa.client.{PageContext, RuleUtils, SendRuleRequest}
import d2spa.client.logger.log
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


// Component not used
object ERDQuery {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDInspect ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDInspect convert EORef into EOs
  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      val cEntityName = currentProps.d2wContext.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.d2wContext.entityName
      val entityNameChanged = !cEntityName.equals(nEntityName)

      val cIsDebugMode = currentProps.proxy.value.appConfiguration.isDebugMode
      val nIsDebugMode = nextProps.proxy.value.appConfiguration.isDebugMode
      val isDebugModeChanged = !cIsDebugMode.equals(nIsDebugMode)



      log.finest("cEntityName " + cEntityName + " nEntityName " + nEntityName)

      val anyChange = entityNameChanged || isDebugModeChanged

      Callback.when(anyChange) {
        mounted(nextProps)
      }
    }



    def mounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      log.finest("PageRepetition | mounted | entityName " + entityName)

      val ruleResults = p.proxy.value.ruleResults
      val ruleRequest = RuleUtils.metaDataRuleRequest(ruleResults, d2wContext)
      Callback.when(!RulesUtilities.isEmptyRuleRequest(ruleRequest))(p.proxy.dispatchCB(SendRuleRequest(ruleRequest)))
    }

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      log.finest("ERDQuery render " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      PageRepetition(p.router,pageContext, p.proxy)

    }
  }

  private val component = ScalaComponent.builder[Props]("ERDQuery")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
