package d2spa.client.components

import d2spa.client.{AppModel, FireRulesForKeys}
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, UpdateQueryProperty}
import d2spa.shared.{PropertyMetaInfo, QueryValue, D2WContext, RuleKeys, QueryOperator}

object ERD2WQueryToOneField  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,


  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])


  // keyWhenRelationship is used for the key to match: propertyKey + "." + keyWhenRelationship
  // displayNameForKeyWhenRelationship = D2WContext(task=query, entity = destinationEntity, propertyKey = keyWhenRelationship).displayNameForProperty
  // (is used for the pre-text)
  // 2 info needed, one based on the other
  // destinationEntity has to be processed in EOF application (D2SPAServer)
  // => we can ask: D2WContext.displayNameForKeyWhenRelationship
  // this component will perform action FireRulesForKeys (keyWhenRelationship,displayNameForKeyWhenRelationship)


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val d2wContext = props.proxy.value.menuModel.get.d2wContext.copy(propertyKey = props.property.d2WContext.propertyKey)
      val dataNotFetched = !AppModel.rulesContainsKey(props.property,RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(props.proxy.dispatchCB(FireRulesForKeys(props.property, List(RuleKeys.keyWhenRelationship, RuleKeys.displayNameForKeyWhenRelationship))))
    }

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val displayNameForKeyWhenRelationship = AppModel.ruleForKey(p.property,RuleKeys.displayNameForKeyWhenRelationship)
      val queryKey = p.property.d2WContext.propertyKey + "." + AppModel.ruleForKey(p.property,RuleKeys.keyWhenRelationship)
      val pretext = "where " + displayNameForKeyWhenRelationship + " is "
      val value = if (p.proxy().queryValues.contains(queryKey)) p.proxy().queryValues(queryKey) else ""
      <.div(
        <.span(pretext),
        <.input(^.id := "toOneTextField", ^.value := p.property.value.value,
           ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entity, QueryValue(queryKey,e.target.value, QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryToOneField")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, proxy: ModelProxy[MegaContent]) = component(Props(ctl,property,proxy))
}
