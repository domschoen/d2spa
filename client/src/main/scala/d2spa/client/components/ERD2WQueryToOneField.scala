package d2spa.client.components

import d2spa.client.{AppModel, FireActions, FireRule}
import d2spa.shared.{EOModelUtils, RuleUtils}
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


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent])


  // keyWhenRelationship is used for the key to match: propertyKey + "." + keyWhenRelationship
  // displayNameForKeyWhenRelationship = D2WContext(task=query, entity = destinationEntity, propertyKey = keyWhenRelationship).displayNameForProperty
  // (is used for the pre-text)
  // 2 info needed, one based on the other
  // destinationEntity has to be processed in EOF application (D2SPAServer)
  // => we can ask: D2WContext.displayNameForKeyWhenRelationship
  // this component will perform action FireRulesForKeys (keyWhenRelationship,displayNameForKeyWhenRelationship)


  class Backend($ : BackendScope[Props, Unit]) {

    def d2wContext(props: Props) = props.d2wContext.copy(propertyKey = Some(props.property.name))

    def mounted(p: Props) = {
      val currentD2wContext = d2wContext(p)
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(p.property, currentD2wContext, RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          p.property,
          List(
            FireRule(currentD2wContext, RuleKeys.keyWhenRelationship),
            FireRule(currentD2wContext, RuleKeys.displayNameForKeyWhenRelationship)
          )
        )
      ))
    }

    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      //val eomodel = p.proxy.value.eomodel.get
      //val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val currentD2wContext = d2wContext(p)
      val displayNameForKeyWhenRelationship = RuleUtils.ruleStringValueForContextAndKey(p.property, currentD2wContext, RuleKeys.displayNameForKeyWhenRelationship)
      val whereDisplayText = displayNameForKeyWhenRelationship match {
        case Some(Some(text)) => text
        case _ => ""
      }

      val queryKey = p.property.name + "." + RuleUtils.ruleStringValueForContextAndKey(p.property,currentD2wContext,RuleKeys.keyWhenRelationship)
      val pretext = "where " + whereDisplayText + " is "
      val queryValue = p.proxy().queryValues.find(r => {r.key.equals(queryKey)})
      val value = if (queryValue.isDefined) queryValue.get.value else ""
      <.div(
        <.span(pretext),
        <.input(^.id := "toOneTextField", ^.value := value,
           ^.onChange ==> {e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName, QueryValue(queryKey,e.target.value, QueryOperator.Match)))} )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryToOneField")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property,proxy))
}
