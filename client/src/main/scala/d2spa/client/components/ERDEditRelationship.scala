package d2spa.client.components


import d2spa.client.{AppModel, FireRules}
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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)

  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val d2wContext = props.proxy.value.menuModel.get.d2wContext.copy(propertyKey = Some(props.property.name))
      val dataNotFetched = !AppModel.rulesContainsKey(props.property,RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(props.proxy.dispatchCB(FireRules(props.property, Map(
        RuleKeys.keyWhenRelationship -> d2wContext,
        RuleKeys.displayNameForKeyWhenRelationship  -> d2wContext))))
    }

    def render(p: Props) = {
      val entityName = p.d2wContext.entityName
      val displayNameForKeyWhenRelationship = AppModel.ruleStringValueForKey(p.property,RuleKeys.displayNameForKeyWhenRelationship)
      val queryKey = p.property.name + "." + AppModel.ruleStringValueForKey(p.property,RuleKeys.keyWhenRelationship)
      val pretext = "where " + displayNameForKeyWhenRelationship + " is "
      val queryValue = p.proxy().queryValues.find(r => {r.key.equals(queryKey)})
      val value = if (queryValue.isDefined) queryValue.get.value else ""
      <.div(
        <.span(pretext),
        <.input(^.id := "toOneTextField", ^.value := value,
          ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName, QueryValue(queryKey,e.target.value, QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDEditRelationship")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property, proxy))
}
