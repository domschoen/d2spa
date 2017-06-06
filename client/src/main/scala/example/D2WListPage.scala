package example

import diode.react.ModelProxy
import diode._
import diode.data._
import diode.util._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import example.css.GlobalStyle

import example.D2SPAMain.{TaskAppPage}
import example.components.ERD2WQueryStringOperator
import spatutorial.shared.EOKeyValueQualifier

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    val componentByName = Map("ERD2WQueryStringOperator" -> ERD2WQueryStringOperator)

    /*def search(router: RouterCtl[TaskAppPage],entity: String) = {

      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(Search(entity)))
    }*/


    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.d2wContext.entity
      val entityMetaData = p.proxy.value.metaDatas.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
      val task = entityMetaData.listTask
      val eos = if (task.eos.isReady) task.eos.get else Vector()
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
                <.td(^.className := "listHeaderReturnButton",<.span(<.img(^.src := "/assets/images/ButtonReturn.gif")))
              ),
                 <.table(^.className :="listRepetition",
                   <.tbody(
                       <.tr(^.className :="listRepetitionColumnHeader",
                         displayPropertyKeys.map(property =>
                           <.td(^.className :="listRepetitionColumnHeader",
                            <.span(^.className :="listRepetitionColumnHeader",property.displayName)
                         )
                       )
                     )
                   ),
                   <.tbody(
                    eos.map(eo =>
                     <.tr(displayPropertyKeys.map(
                       property =>
                         <.td(
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
    }
  }

  private val component = ReactComponentB[Props]("D2WListPage")
    .renderBackend[Backend]
    .componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, entity, proxy))
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