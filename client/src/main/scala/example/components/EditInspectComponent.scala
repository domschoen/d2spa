package example.components

import d2spa.shared.EditInspectProperty
import diode.react.ModelProxy
import example.D2SPAMain.TaskAppPage
import example.MegaContent
import japgolly.scalajs.react.extra.router.RouterCtl

trait EditInspectComponent {

  def apply(ctl: RouterCtl[TaskAppPage], property: EditInspectProperty, proxy: ModelProxy[MegaContent])
}
