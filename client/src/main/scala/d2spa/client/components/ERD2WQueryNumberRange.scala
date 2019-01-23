package d2spa.client.components

import d2spa.client._
import d2spa.shared.{IntValue, StringValue}
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, QueryOperator, UpdateQueryProperty}

object ERD2WQueryNumberRange  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def queryValueChanged(entityName: String, propertyName: String, inputValue: String, queryOperator: String) = {
      val action = if (inputValue.length == 0) {
        ClearQueryProperty(entityName,propertyName, queryOperator)
      }  else {
        UpdateQueryProperty(
          entityName,
          QueryValue(
            propertyName,
            IntValue(inputValue.toInt),
            queryOperator
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
          val strValueFrom = D2WContextUtils.queryValueAsStringForKey(pageContext, propertyName, QueryOperator.Min)
          val strValueTo = D2WContextUtils.queryValueAsStringForKey(pageContext, propertyName, QueryOperator.Max)
          // set id but make it unique: ^.id := "description",
          React.Fragment(
            "from ",
            <.input(^.value := strValueFrom,
              ^.placeholder := "write number",  ^.onChange ==> {e: ReactEventFromInput =>
                queryValueChanged(
                    entityName,
                    propertyName,
                    e.target.value,
                  QueryOperator.Min
                )
              }
            ),
            " to ",
            <.input(^.value := strValueTo,
              ^.placeholder := "write number",  ^.onChange ==> {e: ReactEventFromInput =>
                queryValueChanged(
                  entityName,
                  propertyName,
                  e.target.value,
                  QueryOperator.Max
                )
              }
            ),
            " (Number)"
          )

    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryNumberRange")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
