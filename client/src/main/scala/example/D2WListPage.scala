package example

import diode.react.ModelProxy
import diode.Action
import org.scalajs.dom.ext.KeyCode
import example.css.GlobalStyle

import scalacss.ScalaCssReact._
import example.D2SPAMain.{ListPage, TaskAppPage}
import example.components.ERD2WQueryStringOperator
import d2spa.shared.{EO}

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    val componentByName = Map("ERD2WQueryStringOperator" -> ERD2WQueryStringOperator)


    def returnAction (router: RouterCtl[TaskAppPage],entity: String) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(InstallQueryPage(entity)))
    }
    def inspectEO (eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO("list", eo)))
    }

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val entityMetaData = p.proxy.value.metaDatas.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
      val task = entityMetaData.listTask
      //val eos = if (task.eos.isReady) task.eos.get else Vector()
      val eos = p.proxy.value.eos.get
      println("list task inside " + eos )
      val displayPropertyKeys = task.displayPropertyKeys
      <.div(
        <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
        <.div(^.id:="a",
          <.table(^.className := "listPage",
            <.tbody(
              <.tr(^.className := "listHeader",
                <.td(^.className := "listHeaderEntityName",
                  <.span(^.className := "attribute",eos.size + " " + entityMetaData.displayName)
                ),
                <.td(^.className := "listHeaderReturnButton",<.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router,p.entity))))
              )
            ),
            <.tbody(
              <.tr(
                <.td(
                  <.table(^.className :="listRepetition",
                    <.tbody(
                      <.tr(^.className :="listRepetitionColumnHeader",
                        <.td(),
                        displayPropertyKeys.map(property =>
                          <.td(^.className :="listRepetitionColumnHeader",
                            <.span(^.className :="listRepetitionColumnHeader",property.displayName)
                          )
                        )
                      )
                    ),
                    <.tbody(
                      eos.map(eo =>
                        <.tr(
                          <.td(
                            <.img(^.className := "IconButton",^.src := "/assets/images/Magglass.gif", ^.onClick --> inspectEO(eo)),
                            <.img(^.className := "IconButton",^.src := "/assets/images/Write.gif"),
                            <.img(^.className := "IconButton",^.src := "/assets/images/Clone.gif")
                          ),
                          displayPropertyKeys.map(
                          property =>
                            <.td(^.className := "list1",
                              eo.values(property.key).value
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
    }
  }

  private val component = ReactComponentB[Props]("D2WListPage")
    .renderBackend[Backend]
    //.componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, proxy))
  }
}


/*
                listTask.eos match {
                   case Ready(eos) => {
                      eos.map(eo =>
                        <.tr(displayPropertyKeys.map(
                          property =>
                            <.td(
                              eo.values(property.key).value
                            )
                          )
                        )
                      )
                   }
                   case Empty => {
                      <.tr(
                        <.td(
                          "It's empty"
                        )
                      )
                   }
                   case Pending(_) => {
                      <.tr(
                        <.td(
                          "It's pending"
                        )
                      )
                   }
                }

 */
