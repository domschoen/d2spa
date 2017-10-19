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
import d2spa.client.components.{ERD2WQueryStringOperator, QueryComponent}
import d2spa.shared.{EOKeyValueQualifier, QueryProperty}

object D2WQueryPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      println("entity " + props.entity)
      val entityMetaDataNotFetched = props.proxy().metaDatas.entityMetaDatas.indexWhere(n => n.entityName == props.entity) < 0
      println("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      Callback.when(entityMetaDataNotFetched)(props.proxy.dispatchCB(InitMetaData(props.entity)))
    }


    val componentByName = Map("ERD2WQueryStringOperator" -> ERD2WQueryStringOperator)

    def search(router: RouterCtl[TaskAppPage],entity: String,qualifiers: List[EOKeyValueQualifier]) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(Search(entity,qualifiers)))
    }

    def qualifiers(propertyKeys: List[QueryProperty]): List[EOKeyValueQualifier] = {
       propertyKeys.filter(p => p.value.value.length > 0).map(p => EOKeyValueQualifier(p.key,p.value.value))
    }

    def render(p: Props) = {
      val entity = p.entity
      println("Render Query page for entity: " + entity)
      val metaDatas = p.proxy.value.metaDatas
      if  (!metaDatas.isEmpty) {
        val entityMetaData = metaDatas.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
        val task = entityMetaData.queryTask
        val displayPropertyKeys = task.displayPropertyKeys
        <.div(
          <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
          <.div(^.id:="a",
            <.div(^.className := "banner d2wPage",
              <.span(<.img(^.src := "/assets/images/SearchBan.gif"))
            ),
            <.div(^.className :="liner d2wPage",<.img(^.src := "/assets/images/Line.gif")),
            <.div(^.className :="buttonsbar d2wPage",
              <.span(^.className :="buttonsbar attribute beforeFirstButton",entityMetaData.displayName),
              <.span(^.className :="buttonsbar",
                <.img(^.src := "/assets/images/ButtonSearch.gif",^.onClick --> search(p.router,p.entity,qualifiers(displayPropertyKeys)))
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
                              <.th(^.className :="propertyName query",
                                property.displayName
                              ),
                              <.td(^.className :="query d2wAttributeValueCell",
                                ERD2WQueryStringOperator(p.router,property,p.proxy)
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

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, proxy))
  }
}
