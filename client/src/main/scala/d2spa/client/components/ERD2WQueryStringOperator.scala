package d2spa.client.components

import d2spa.client.AppModel
import d2spa.shared.{D2WContext, PropertyMetaInfo, QueryOperator, RuleKeys}
import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react.{ReactEventFromInput, _}
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode

import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.{TaskAppPage}
import d2spa.client.MegaContent
import d2spa.client.UpdateQueryProperty
import d2spa.shared.{ EOValue, EOKeyValueQualifier, PropertyMetaInfo, QueryValue}

object ERD2WQueryStringOperator  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      println("ERD2WQueryStringOperator " + p.property)
      val propertyKey = p.property.name
      val queryValue = p.proxy().queryValues.find(r => {r.key.equals(propertyKey)})
      val value = if (queryValue.isDefined) queryValue.get.value else ""
      <.div(
        <.input(^.id := "description", ^.value := value,
          ^.placeholder := "write description", ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName,
            QueryValue(propertyKey,e.target.value,QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryStringOperator")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property,proxy))
}
