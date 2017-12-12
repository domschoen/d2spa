package d2spa.client.components


import d2spa.client.{AppModel, HydrateProperty}
import d2spa.shared.EO
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, UpdateQueryProperty}
import d2spa.shared.{PropertyMetaInfo, QueryValue, D2WContext, RuleKeys, QueryOperator}

object ERDEditRelationship  {

  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)

  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val d2wContext = props.proxy.value.menuModel.get.d2wContext.copy(propertyKey = props.property.d2wContext.propertyKey)
      val dataNotFetched = !AppModel.rulesContainsKey(props.property,RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(props.proxy.dispatchCB(HydrateProperty(props.property, List(RuleKeys.keyWhenRelationship, RuleKeys.displayNameForKeyWhenRelationship))))
    }

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val displayNameForKeyWhenRelationship = AppModel.ruleStringValueForKey(p.property,RuleKeys.displayNameForKeyWhenRelationship)
      val queryKey = p.property.d2wContext.propertyKey + "." + AppModel.ruleStringValueForKey(p.property,RuleKeys.keyWhenRelationship)
      val pretext = "where " + displayNameForKeyWhenRelationship + " is "
      val queryValue = p.proxy().queryValues.find(r => {r.key.equals(queryKey)})
      val value = if (queryValue.isDefined) queryValue.get.value else ""
      <.div(
        <.span(pretext),
        <.input(^.id := "toOneTextField", ^.value := value,
          ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entity, QueryValue(queryKey,e.target.value, QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDEditRelationship")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))
}
