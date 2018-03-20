package d2spa.client.components


import d2spa.client._
import d2spa.shared.EO
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, UpdateQueryProperty}
import d2spa.shared.{PropertyMetaInfo, QueryValue, RuleKeys, QueryOperator}

object ERDEditRelationship  {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)

  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      val propertyName = p.property.name
      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(p.property, d2wContext, RuleKeys.keyWhenRelationship)

      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(p.property,
          List(
            FireRule(d2wContext, RuleKeys.keyWhenRelationship),
            FireRule(d2wContext, RuleKeys.displayNameForKeyWhenRelationship)
          )
        )
      ))
    }

    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      val propertyName = p.property.name
      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))

      val displayNameForKeyWhenRelationship = RuleUtils.ruleStringValueForContextAndKey(p.property, d2wContext, RuleKeys.displayNameForKeyWhenRelationship).get
      val keyWhenRelationship = RuleUtils.ruleStringValueForContextAndKey(p.property, d2wContext, RuleKeys.keyWhenRelationship).get

      val queryKey = p.property.name + "." + keyWhenRelationship
      val pretext = "where " + displayNameForKeyWhenRelationship + " is "
      val queryValues = p.d2wContext.queryValues
      val queryValue = queryValues.find(r => {r.key.equals(queryKey)})
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
