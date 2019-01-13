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
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


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
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
          val entityName = d2wContext.entityName.get
          val propertyName = d2wContext.propertyKey.get

          //log.finest("ERD2WQueryStringOperator " + propertyName + " query values " + d2wContext.queryValues)
          val strValue = D2WContextUtils.queryValueAsStringForKey(pageContext, propertyName)
          // set id but make it unique: ^.id := "description",
          <.div(
            <.input(^.value := strValue, ^.className := "form-control",
              ^.placeholder := "write description",  ^.onChange ==> {e: ReactEventFromInput =>
                queryValueChanged(
                    entityName,
                    propertyName,
                    e.target.value)
              }
            )
          )

    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryStringOperator")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
