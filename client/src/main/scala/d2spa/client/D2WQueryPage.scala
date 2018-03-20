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



    def mounted(props: Props) = {
      val entityName = props.d2wContext.entityName.get
      log.debug("D2WQueryPage " + entityName)

      val entityMetaData = props.proxy().entityMetaDatas.find(emd => emd.entity.name.equals(entityName))
      log.debug("entityMetaDataNotFetched " + entityMetaData)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity

      Callback.when(entityMetaData.isEmpty)(props.proxy.dispatchCB(InitMetaData(entityName)))
    }



    def search(router: RouterCtl[TaskAppPage],entity: EOEntity) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(Search(entity)))
    }


    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      log.debug("Render Query page for entity: " + entityName)
      val entityMetaData = p.proxy.value.entityMetaDatas.find(emd => emd.entity.name.equals(entityName))
      entityMetaData match {
        case Some(entityMetaData) =>

        val task = entityMetaData.queryTask
        val displayPropertyKeys = task.displayPropertyKeys
        <.div(
          <.div(^.id:="b",MenuHeader(p.router, entityName, p.proxy)),
          <.div(^.id:="a",
            <.div(^.className := "banner d2wPage",
              <.span(<.img(^.src := "/assets/images/SearchBan.gif"))
            ),
            <.div(^.className :="liner d2wPage",<.img(^.src := "/assets/images/Line.gif")),
            <.div(^.className :="buttonsbar d2wPage",
              <.span(^.className :="buttonsbar attribute beforeFirstButton",entityMetaData.displayName),
              <.span(^.className :="buttonsbar",
                <.img(^.src := "/assets/images/ButtonSearch.gif",^.onClick --> search(p.router,entityMetaData.entity))
                //p.router.link(ListPage(p.entity))("Search")
              )
            ),
            <.div(^.className :="repetition d2wPage",
              <.table(^.className :="query",
                <.tbody(
                  <.tr(^.className :="attribute customer",
                    <.td(
                      <.table(
                        <.tbody(
                          displayPropertyKeys toTagMod (property => {
                            val d2wContext = p.d2wContext.copy(propertyKey = Some(property.name))
                            <.tr(^.className :="attribute",
                              <.th(^.className :="propertyName query",{
                                val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(property,d2wContext, RuleKeys.displayNameForProperty)
                                displayNameFound match {
                                  case Some(stringValue) => stringValue
                                  //case Some(stringValue) => stringValue
                                  case _ => ""
                                }
                              }
                              ),
                              // Component part
                              <.td(^.className := "query d2wAttributeValueCell",
                                D2WComponentInstaller(p.router, d2wContext, property, null, p.proxy)
                              )


                            )    }
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
