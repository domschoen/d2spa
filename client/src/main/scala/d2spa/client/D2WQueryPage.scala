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
import d2spa.client.components.{ERD2WQueryStringOperator, ERD2WQueryToOneField}
import d2spa.shared._

object D2WQueryPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val entityName = props.d2wContext.entityName.get
      println("D2WQueryPage " + entityName)

      val entityMetaDataNotFetched = props.proxy().entityMetaDatas.indexWhere(n => n.entity.name.equals(entityName)) < 0
      println("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity

      Callback.when(entityMetaDataNotFetched)(props.proxy.dispatchCB(InitMetaData(entityName)))
    }



    def search(router: RouterCtl[TaskAppPage],entity: EOEntity) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(Search(entity)))
    }


    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      println("Render Query page for entity: " + entityName)
      val metaDatas = p.proxy.value
      if  (!metaDatas.entityMetaDatas.isEmpty) {
        val entityMetaData = metaDatas.entityMetaDatas.find(emd => emd.entity.name.equals(entityName)).get
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
                          displayPropertyKeys toTagMod (property =>
                            <.tr(^.className :="attribute",
                              <.th(^.className :="propertyName query",{
                                val d2wContext = p.d2wContext.copy(propertyKey = Some(property.name))
                                val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(property,d2wContext, RuleKeys.displayNameForProperty)
                                displayNameFound match {
                                  case Some(Some(stringValue)) => stringValue
                                  case _ => ""
                                }
                              }
                              ),
                              // Component part
                              <.td(^.className :="query d2wAttributeValueCell",
                                {
                                  val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(property.name))
                                  val componentNameFound = RuleUtils.ruleStringValueForContextAndKey(property,propertyD2WContext, RuleKeys.componentName)
                                  componentNameFound match {
                                    case Some(Some(componentName)) =>
                                      componentName match {

                                        case "ERD2WQueryStringOperator" => ERD2WQueryStringOperator (p.router, propertyD2WContext, property, p.proxy)
                                        case "ERD2WQueryToOneField" => ERD2WQueryToOneField (p.router, propertyD2WContext, property, p.proxy)
                                        case _ => "Component not found: " + componentName
                                      }
                                    case _ => "Component Name rule not found for property: " + property.name

                                  }
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
          )
        )
      } else {
        <.div("no meta datas")
      }
    }
  }


  //                               //componentByName(property.componentName).asInstanceOf[QueryComponent](p.router,property,p.proxy)


  private val component = ScalaComponent.builder[Props]("D2WQueryPage")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, d2wContext, proxy))
  }
}
