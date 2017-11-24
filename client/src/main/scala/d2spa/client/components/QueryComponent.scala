package d2spa.client.components

import d2spa.shared.{PropertyMetaInfo}
import diode.react.ModelProxy
import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.MegaContent
import d2spa.client.components.ERD2WQueryStringOperator.{Props, component}
import japgolly.scalajs.react.extra.router.RouterCtl

trait QueryComponent {


  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])

}
