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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def willmounted(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get

      val entityMetaDataNotFetched = !RuleUtils.metaDataFetched(p.proxy().ruleResults, d2wContext)
      log.debug("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      Callback.when(entityMetaDataNotFetched)(p.proxy.dispatchCB(InitMetaDataForList(entityName)))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
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

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = {
    log.debug("ctl " + ctl.hashCode())
    component(Props(ctl, d2wContext, proxy))
  }
}

