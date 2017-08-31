package example

import d2spa.shared.{EOKeyValueQualifier, EditInspectProperty, QueryProperty}
import diode.react.ModelProxy
import example.D2SPAMain.TaskAppPage
import example.components.{ERD2WQueryStringOperator, ERD2WEditNumber, ERD2WEditString, EditInspectComponent}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import d2spa.shared.EO

object D2WEditPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      Callback.when(props.proxy().metaDatas.isEmpty)(props.proxy.dispatchCB(InitMenu))
    }

    val componentByName = Map(
      "EditInspectProperty" -> EditInspectProperty,
      "ERD2WEditNumber" -> ERD2WEditNumber
    )

    def save(router: RouterCtl[TaskAppPage],entity: String,eo: EO) = {
      Callback.log(s"Save: $entity") >>
        $.props >>= (_.proxy.dispatchCB(Save(entity,eo)))
    }

    def eo(propertyKeys: List[EditInspectProperty]): EO = {
       //propertyKeys.filter(p => p.value.value.length > 0).map(p => EOKeyValueQualifier(p.key,p.value.value))
      EO(Map())
    }

    def render(p: Props) = {
      val entity = p.entity
      println("Render Query page for entity: " + entity)
      val metaDatas = p.proxy.value.metaDatas
      if  (!metaDatas.isEmpty) {
        val entityMetaData = metaDatas.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
        val task = entityMetaData.editTask
        val displayPropertyKeys = task.displayPropertyKeys
        val banImage = if (task.equals("edit")) "/assets/images/EditBan.gif" else "/assets/images/InspectBan.gif"
        <.div(
          <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
          <.div(^.id:="a",
            <.div(^.className := "banner d2wPage",
              <.span(<.img(^.src := banImage))
            ),
            <.div(^.className :="liner d2wPage",<.img(^.src := "/assets/images/Line.gif")),
            <.div(^.className :="buttonsbar d2wPage",
              <.span(^.className :="buttonsbar attribute beforeFirstButton",entityMetaData.displayName),
              <.span(^.className :="buttonsbar",
                <.img(^.src := "/assets/images/ButtonSave.gif",^.onClick --> save(p.router,p.entity,eo(displayPropertyKeys)))
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
                        displayPropertyKeys.map(property =>
                          <.tr(^.className :="attribute",
                            <.th(^.className :="propertyName query",
                              property.displayName
                            ),
                            <.td(^.className :="query d2wAttributeValueCell",
                              //componentByName(property.componentName).asInstanceOf[EditInspectComponent](p.router,property,p.proxy)
                              ERD2WEditString(p.router,property,p.proxy)
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

  private val component = ReactComponentB[Props]("D2WEditPage")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    //.componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, task, proxy))
  }
}
