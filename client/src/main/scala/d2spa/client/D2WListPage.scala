package d2spa.client


import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{Button, CommonStyle}
import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.react.ModelProxy
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.components.ERDList.Props
import d2spa.client.components.{ERD2WQueryStringOperator, ERD2WQueryToOneField, NVListComponent}
import d2spa.client.logger.log
import d2spa.shared._
import diode.data.Ready

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {


    def willmounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get

      val allFirings = RuleUtils.metaDataFiringRules(p.proxy.value.ruleResults, d2wContext)

      if (allFirings.isEmpty) {
        Callback.empty
      } else {
        val ruleRequest = RuleRequest(d2wContext,allFirings)
        p.proxy.dispatchCB(SendRuleRequest(ruleRequest))
      }
    }

    def render(p: Props) = {
      log.finest("D2WListPage render")
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      <.div(
        <.div(^.id:="b",MenuHeader(p.router,entityName,p.proxy)),
        <.div(^.id:="a",NVListComponent(p.router,p.d2wContext,false,p.proxy))
        //<.div(^.id:="a","simply")
      )
    }
  }


  private val component = ScalaComponent.builder[Props]("D2WListPage")
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = {
    component(Props(ctl, d2wContext, proxy))
  }
}

