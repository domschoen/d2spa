package d2spa.client


import diode.data.Pot
import diode.react._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.GlobalStyles

import scalacss.ScalaCssReact._
import d2spa.client.SPAMain.{ListPage, QueryPage, TaskAppPage}
import d2spa.client.components.Bootstrap.{Button, CommonStyle}
import d2spa.client.components.Icon



object MenuHeader {
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])


  case class State(motdWrapper: ReactConnectProxy[Pot[String]])

  class Backend($: BackendScope[Props, Unit]) {
    /*def mounted(props: Props) = {
      // dispatch a message to the initial D2WData from the server
      Callback.when(props.proxy().menuModel.isEmpty)(props.proxy.dispatchCB(InitMenu))
    }*/

    def selectMenu(entity: String) = {
      println("selectMenu")

      Callback.log(s"Menu selected: $entity") >>
        $.props >>= (_.proxy.dispatchCB(SelectMenu(entity)))
    }

    def newEO(entity: String) = {
      println("new EO for entity " + entity)

      Callback.log(s"New EO for: $entity") >>
        $.props >>= (_.proxy.dispatchCB(NewEOPage(entity)))
    }


    def render(p: Props) = {
      val style = bss.listGroup
      val debugButtonText = if (p.proxy.value.isDebugMode) "Turn off D2W Debug" else "Turn on D2W Debug"
      <.div(
        if (!p.proxy.value.menuModel.isEmpty) {
          <.div(
            <.ul(style.listGroup, ^.className := "menu",
              p.proxy.value.menuModel.get.menus toTagMod (mainMenu =>
                mainMenu.children toTagMod (
                  menu => {
                    <.li(style.item, GlobalStyles.menuItem, (style.active).when(p.entity.equals(menu.entity)),
                      <.div(GlobalStyles.menuInputGroup,
                        <.div(GlobalStyles.menuLabel, menu.entity, ^.onClick --> selectMenu(menu.entity)),
                        <.div(GlobalStyles.menuAddon, <.i(^.className := "fa fa-plus"), ^.onClick --> newEO(menu.entity))
                      )
                    )
                  }
                )
              )
            ),
            Button(Button.Props(p.proxy.dispatchCB(SwithDebugMode), CommonStyle.danger), Icon.bug, debugButtonText)
          )
        } else
          ""
      )

    }
  }

  private val component = ScalaComponent.builder[Props]("MenuHeader")
    .renderBackend[Backend]
    .build


  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, entity, proxy))
}