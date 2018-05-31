package d2spa.client.components

import d2spa.client._
import d2spa.client.logger.log
import d2spa.shared.StringValue
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
import d2spa.shared.{ EOValue, EOKeyValueQualifier, PropertyMetaInfo, RuleKeys}

object ERD2WQueryStringOperator  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def queryValueChanged(entityName: String, propertyName: String, inputValue: String) = {
      val action = if (inputValue.length == 0) {
        ClearQueryProperty(entityName,propertyName)
      }  else {
        UpdateQueryProperty(
          entityName,
          QueryValue(
            propertyName,
            StringValue(inputValue),
            QueryOperator.Match
          )
        )
      }
      Callback.log(s"Set to Yes : $propertyName") >>
        $.props >>= (_.proxy.dispatchCB(action))
    }


    def render(p: Props) = {
      val d2wContextOpt = p.proxy.value.previousPage
      d2wContextOpt match {
        case Some(d2wContext) =>
          val propertyD2WContext = p.d2wContext
          val entityName = propertyD2WContext.entityName.get
          val propertyName = propertyD2WContext.propertyKey.get

          log.debug("ERD2WQueryStringOperator " + propertyName + " query values " + d2wContext.queryValues)
          val strValue = D2WContextUtils.queryValueAsStringForKey(d2wContext, propertyName)
          // set id but make it unique: ^.id := "description",
          <.div(
            <.input(^.value := strValue,
              ^.placeholder := "write description",  ^.onChange ==> {e: ReactEventFromInput =>
                queryValueChanged(
                    entityName,
                    propertyName,
                    e.target.value)
              }
            )
          )
        case _ =>
          <.div("no context")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryStringOperator")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
