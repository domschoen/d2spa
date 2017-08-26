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
    def mounted(props: Props) =
    // dispatch a message to the initial D2WData from the server
      Callback.when(props.proxy().menuModel.isEmpty)(props.proxy.dispatchCB(InitMenu))

    def selectMenu(entity: String) = {
      println("selectMenu")

      Callback.log(s"Menu selected: $entity") >>
        $.props >>= (_.proxy.dispatchCB(SelectMenu(entity)))
    }

    def render(p: Props) = {
      //val callbacks = Callbacks(P)
      // + P.proxy.value.menuModel.d2wContext.entity
      <.div(
        <.img(^.src := "/assets/images/LogoIST.gif"),
        <.h2(^.id := "version", <.span("FOSS-it 1.2.2")),
        <.h3(^.className := "section", "Entities:"),
        //<.p(P.proxy.value.toString()),
        if (!p.proxy.value.menuModel.isEmpty) {
          <.ul(
            p.proxy.value.menuModel.get.menus.map(mainMenu =>
              <.li(mainMenu.title,
                <.ul(^.className := "action", mainMenu.children.map(
                  menu => {
                    val menuCss = if (p.proxy.value.menuModel.get.d2wContext.entity.equals(menu.entity)) "menuSelected" else "menu"
                    <.li(^.className := menuCss, ^.onClick --> selectMenu(menu.entity), menu.entity)
                  }
                )
                )
              )
            )
          )
        } else
          Seq.empty[ReactElement],
        <.img(^.src := "/assets/images/New.gif",^.onClick --> selectMenu(p.entity))


      )
    }
  }

  private val component = ReactComponentB[Props]("MenuHeader")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, entity, proxy))
}