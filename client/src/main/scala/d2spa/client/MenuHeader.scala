package d2spa.client


import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{CommonStyle, Button}
import scalacss.ScalaCssReact._
import japgolly.scalajs.react.vdom.Exports

import diode.react.ModelProxy
import diode.Action
import org.scalajs.dom.ext.KeyCode
import d2spa.client.SPAMain.{ListPage, QueryPage, TaskAppPage}
import d2spa.client.components.GlobalStyles
import d2spa.client.components.Icon._
import d2spa.client.components._


object MenuHeader {
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent])

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
      //val callbacks = Callbacks(P)
      // + P.proxy.value.menuModel.d2wContext.entity
      <.div(
        <.img(^.src := "/assets/images/LogoIST.gif"),
        <.h2(^.id := "version", <.span("FOSS-it 1.2.2")),
        <.h3(^.className := "section", "Entities:"),
        //<.span(bss.labelOpt(CommonStyle.danger), bss.labelAsBadge, 1),
        //<.p(P.proxy.value.toString()),
        if (!p.proxy.value.menuModel.isEmpty) {
          <.ul(
            p.proxy.value.menuModel.get.menus toTagMod (mainMenu =>
              <.li(mainMenu.title,
                <.ul(^.className := "action", mainMenu.children toTagMod (
                  menu => {
                    //val menuCss = if (p.proxy.value.menuModel.get.d2wContext.entity.equals(menu.entity)) "menuSelected" else "menu"
                    val menuCss = if (p.entity.equals(menu.entity)) "menuSelected" else "menu"
                    <.li(^.className := menuCss, ^.onClick --> selectMenu(menu.entity), menu.entity)
                  }
                )
                )
              )
            )
          )
        } else
          EmptyVdom,
        <.img(^.src := "/assets/images/New.gif",^.onClick --> newEO(p.entity))


      )
    }
  }

  private val component = ScalaComponent.builder[Props]("MenuHeader")
    .renderBackend[Backend]
    //.componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, entity, proxy))
}