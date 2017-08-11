package example

import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import example.D2SPAMain.{QueryPage,ListPage,TaskAppPage}

object MenuHeader {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])

  class Backend($: BackendScope[Props, Unit]) {

    def selectMenu(router: RouterCtl[TaskAppPage],entity: String) = {

      Callback.log(s"Menu selected: $entity") >>
        $.props >>= (_.proxy.dispatchCB(SelectMenu(entity,router)))
    }

    def render(P: Props) = {
      //val callbacks = Callbacks(P)
      // + P.proxy.value.menuModel.d2wContext.entity
      <.div(
        <.img(^.src := "/assets/images/LogoIST.gif"),
        <.h2(^.id := "version", <.span("FOSS-it 1.2.2")),
        <.h3(^.className := "section", "Entities:"),
        //<.p(P.proxy.value.toString()),
        <.ul(
          P.proxy.value.menuModel.menus.map(mainMenu =>
            <.li(mainMenu.title,
              <.ul(^.className := "action", mainMenu.children.map(
                menu =>
                  <.li(^.onClick --> selectMenu(P.router,menu.entity),menu.entity)
                  //<.li(P.router.link(QueryPage(menu.entity))(menu.entity))
              )
              )
            )
          )
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("MenuHeader")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, entity, proxy))
}