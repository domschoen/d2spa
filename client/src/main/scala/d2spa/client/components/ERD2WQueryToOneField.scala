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


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


  // keyWhenRelationship is used for the key to match: propertyKey + "." + keyWhenRelationship
  // displayNameForKeyWhenRelationship = D2WContext(task=query, entity = destinationEntity, propertyKey = keyWhenRelationship).displayNameForProperty
  // (is used for the pre-text)
  // 2 info needed, one based on the other
  // destinationEntity has to be processed in EOF application (D2SPAServer)
  // => we can ask: D2WContext.displayNameForKeyWhenRelationship
  // this component will perform action FireRulesForKeys (keyWhenRelationship,displayNameForKeyWhenRelationship)


  class Backend($ : BackendScope[Props, Unit]) {


    def mounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val ruleResultsModel = p.proxy.value.ruleResults

      val ruleResults = p.proxy.value.ruleResults

      val metaDataRules = RuleUtils.metaDataFiringRules(p.proxy.value.ruleResults, d2wContext)
      val keyWhenRelationshipRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
      val additionalRulesPots = List(keyWhenRelationshipRuleResultPot)
      val additionalRules = RuleUtils.firingRulesFromPotFiredRuleResult(additionalRulesPots)


      val rules = metaDataRules ::: additionalRules

      val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)
      Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))
    }

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
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
          val value = D2WContextUtils.queryValueAsStringForKey(pageContext, propertyName)
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

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
