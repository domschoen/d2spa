package example.components

import d2spa.shared.QueryProperty
import diode.react.ModelProxy
import example.D2SPAMain.TaskAppPage
import example.MegaContent
import example.components.ERD2WQueryStringOperator.{Props, component}
import japgolly.scalajs.react.extra.router.RouterCtl

trait QueryComponent {


  def apply(ctl: RouterCtl[TaskAppPage], property: QueryProperty, proxy: ModelProxy[MegaContent])

}
