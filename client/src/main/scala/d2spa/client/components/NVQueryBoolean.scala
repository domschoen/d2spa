package d2spa.client.components

import d2spa.client._
import d2spa.client.logger.log
import d2spa.shared.BooleanValue
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
import d2spa.client.QueryOperator
import d2spa.shared.{EOValue, EOKeyValueQualifier, PropertyMetaInfo, RuleKeys}

object NVQueryBoolean {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($: BackendScope[Props, Unit]) {


    def setDontCare(entityName: String, propertyName: String) = {
      Callback.log(s"Set to Yes : $propertyName") >>
        $.props >>= (_.proxy.dispatchCB(ClearQueryProperty(entityName, propertyName)))
    }

    def setYes(entityName: String, propertyName: String) = {
      Callback.log(s"Set to Yes : $propertyName") >>
        $.props >>= (_.proxy.dispatchCB(UpdateQueryProperty(entityName,
        QueryValue(propertyName, BooleanValue(true), QueryOperator.Match))))
    }

    def setNo(entityName: String, propertyName: String) = {
      Callback.log(s"Set to Yes : $propertyName") >>
        $.props >>= (_.proxy.dispatchCB(UpdateQueryProperty(entityName,
        QueryValue(propertyName, BooleanValue(false), QueryOperator.Match))))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val propertyD2WContext = p.d2wContext
      val entityName = propertyD2WContext.entityName.get
      val propertyName = propertyD2WContext.propertyKey.get

      log.debug("NVQueryBoolean " + propertyName + " query values " + d2wContext.queryValues)
      val (dontCare, isYes, isNo) = D2WContextUtils.queryValueForKey(d2wContext, propertyName) match {
        case Some(BooleanValue(value)) =>
          log.debug("NVQueryBoolean value " + value)
          if (value) (false, true, false) else (false, false, true)
        case _ => (true, false, false)
      }
      <.div(
        <.input.radio(^.id := "description", ^.checked := dontCare, ^.onChange --> setDontCare(entityName, propertyName)), <.span("don't care"),
        <.input.radio(^.id := "description", ^.checked := isYes, ^.onChange --> setYes(entityName, propertyName)), <.span("yes"),
        <.input.radio(^.id := "description", ^.checked := isNo, ^.onChange --> setNo(entityName, propertyName)), <.span("no")

        //  ^.placeholder := "write description", ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName,
        //    QueryValue(propertyName,e.target.value, QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("NVQueryBoolean")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}

