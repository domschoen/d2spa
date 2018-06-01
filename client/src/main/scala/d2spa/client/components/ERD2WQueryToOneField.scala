package d2spa.client.components

import d2spa.client._
import d2spa.shared.{EOModelUtils, EOValue}
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.{MegaContent, UpdateQueryProperty}
import d2spa.shared.{PropertyMetaInfo, RuleKeys}

object ERD2WQueryToOneField  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
//bss.formControl,


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  // keyWhenRelationship is used for the key to match: propertyKey + "." + keyWhenRelationship
  // displayNameForKeyWhenRelationship = D2WContext(task=query, entity = destinationEntity, propertyKey = keyWhenRelationship).displayNameForProperty
  // (is used for the pre-text)
  // 2 info needed, one based on the other
  // destinationEntity has to be processed in EOF application (D2SPAServer)
  // => we can ask: D2WContext.displayNameForKeyWhenRelationship
  // this component will perform action FireRulesForKeys (keyWhenRelationship,displayNameForKeyWhenRelationship)


  class Backend($ : BackendScope[Props, Unit]) {


    def mounted(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val ruleResultsModel = p.proxy.value.ruleResults

      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          List(
            FireRule(d2wContext, RuleKeys.keyWhenRelationship),
            FireRule(d2wContext, RuleKeys.displayNameForKeyWhenRelationship)
          )
        )
      ))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val ruleResultsModel = p.proxy.value.ruleResults

      val displayNameForKeyWhenRelationship = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForKeyWhenRelationship)
      val whereDisplayText = displayNameForKeyWhenRelationship match {
        case Some(text) => text
        case _ => ""
      }

      val keyWhenRelationshipOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
      keyWhenRelationshipOpt match {
        case Some(keyWhenRelationship) => {

          val queryKey = propertyName + "." + keyWhenRelationship
          val pretext = "where " + whereDisplayText + " is "
          val value = D2WContextUtils.queryValueAsStringForKey(d2wContext, propertyName)
          <.div(
            <.span(pretext),
            <.input(^.id := "toOneTextField", ^.value := value,
              ^.onChange ==> { e: ReactEventFromInput => p.proxy.dispatchCB(UpdateQueryProperty(entityName, QueryValue(queryKey, EOValue.eoValueWithString(e.target.value), QueryOperator.Match))) })
          )
        }
        case _ => <.div("Missing query values")
      }

    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WQueryToOneField")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
