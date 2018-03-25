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
import d2spa.client.components.{D2WComponentInstaller, ERD2WQueryStringOperator, ERD2WQueryToOneField}
import d2spa.shared._
import d2spa.client.logger._

object D2WQueryPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {


    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityNameChanged = !cEntityName.equals(nEntityName)

      val cIsDebugMode = currentProps.proxy.value.isDebugMode
      val nIsDebugMode = nextProps.proxy.value.isDebugMode
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
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(InitMetaData(entityName)))
    }



    def search(router: RouterCtl[TaskAppPage],entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(Search(entityName)))
    }


    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      log.debug("Render Query page for entity: " + entityName)
      val ruleResultsModel = p.proxy.value.ruleResults

      val ruleContainerOpt = RuleUtils.ruleContainerForContext(ruleResultsModel,d2wContext)

      ruleContainerOpt match {
        case Some(ruleContainer) => {

          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

          entityDisplayNameOpt match {
            case Some(entityDisplayName) =>
              <.div(
                <.div(^.id := "b", MenuHeader(p.router, entityName, p.proxy)),
                <.div(^.id := "a",
                  <.div(^.className := "banner d2wPage",
                    <.span(<.img(^.src := "/assets/images/SearchBan.gif"))
                  ),
                  <.div(^.className := "liner d2wPage", <.img(^.src := "/assets/images/Line.gif")),
                  <.div(^.className := "buttonsbar d2wPage",
                    <.span(^.className := "buttonsbar attribute beforeFirstButton", entityDisplayName),
                    <.span(^.className := "buttonsbar",
                      <.img(^.src := "/assets/images/ButtonSearch.gif", ^.onClick --> search(p.router, entityName))
                      //p.router.link(ListPage(p.entity))("Search")
                    )
                  ),
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


  //                               //componentByName(property.componentName).asInstanceOf[QueryComponent](p.router,property,p.proxy)


  private val component = ScalaComponent.builder[Props]("D2WQueryPage")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps,scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = {
    log.debug("ctl " + ctl.hashCode())
    component(Props(ctl, d2wContext, proxy))
  }
}
