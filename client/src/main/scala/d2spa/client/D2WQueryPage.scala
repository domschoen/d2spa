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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {


    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      /*val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityNameChanged = !cEntityName.equals(nEntityName)

      val cIsDebugMode = currentProps.proxy.value.appConfiguration.isDebugMode
      val nIsDebugMode = nextProps.proxy.value.appConfiguration.isDebugMode
      val isDebugModeChanged = !cIsDebugMode.equals(nIsDebugMode)



      log.finest("cEntityName " + cEntityName + " nEntityName " + nEntityName)

      val anyChange = entityNameChanged || isDebugModeChanged*/

      //Callback.when(anyChange) {
      //  mounted(nextProps)
      //}
      mounted(nextProps)

    }



    def mounted(p: Props) = {
      /*val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      log.finest("D2WQueryPage | mounted " + entityName)

      val ruleResults = p.proxy.value.ruleResults
      val socketReady = p.proxy.value.appConfiguration.socketReady
      val dataNotFetched = socketReady && !RuleUtils.metaDataFetched(ruleResults, d2wContext)
      log.finest("D2WQueryPage | mounted | socketReady: " + socketReady + " dataNotFetched: " + dataNotFetched)
      val sendingAction = InitMetaData(d2wContext)
      val alreadySent = p.proxy.value.sendingActions.contains(sendingAction)

      val previousPage = p.proxy.value.previousPage
      val previousPageHasBeenSet = previousPage match {
        case Some(ppD2WContext) => ppD2WContext.equals(d2wContext)
        case None => false
      }
      log.finest("D2WQueryPage | mounted | previousPageHasBeenSet: " + previousPageHasBeenSet + " dataNotFetched && !alreadySent: " + (dataNotFetched && !alreadySent))
      log.finest("D2WQueryPage | mounted | alreadySent: " + alreadySent + " dataNotFetched: " + dataNotFetched)

      if (previousPageHasBeenSet) {
        Callback.when(dataNotFetched && !alreadySent)(p.proxy.dispatchCB(SendingAction(sendingAction)))
      } else {
        p.proxy.dispatchCB(RegisterPreviousPage(d2wContext))
      }*/
      Callback.empty
    }



    def search(router: RouterCtl[TaskAppPage],entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SearchAction(entityName)))
    }


    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      log.finest("Render Query page for entity: " + entityName)
      val ruleResultsModel = p.proxy.value.ruleResults

      val ruleContainerOpt = RuleUtils.ruleContainerForContext(ruleResultsModel,d2wContext)

      ruleContainerOpt match {
        case Some(ruleContainer) => {
          log.finest("Render Query ruleContainer: " + ruleContainer)

          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)
          log.finest("Render Query displayPropertyKeys: " + displayPropertyKeys)

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
                    <.table(^.width := "100%", ^.className := "query",
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

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = {
    component(Props(ctl, d2wContext, proxy))
  }
}
