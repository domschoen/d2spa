package example

import diode.react.ModelProxy
import diode.Action
import org.scalajs.dom.ext.KeyCode
import example.css.GlobalStyle
import scalacss.ScalaCssReact._

import example.D2SPAMain.{ListPage, TaskAppPage}
import example.components.ERD2WQueryStringOperator
import d2spa.shared.{EOKeyValueQualifier}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._

object D2WPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
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
      val entityMetaData = p.proxy.value.metaDatas.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
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
              <.tbody(<.tr(^.className :="attribute customer",
                <.table(
                displayPropertyKeys.map(property =>
                    <.tr(^.className :="attribute",
                      <.th(^.className :="propertyName query",
                        property.displayName
                      ),
                      <.td(^.className :="query d2wAttributeValueCell",
                        componentByName(property.componentName)(p.router,property,p.proxy)
                      )
                    )
                  )
                )
              )
            ))
            )
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("D2WPage")
    .renderBackend[Backend]
    //.componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, proxy))
  }
}
