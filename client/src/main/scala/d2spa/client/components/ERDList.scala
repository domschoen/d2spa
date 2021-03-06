package d2spa.client.components

import d2spa.client._
import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
import diode.data.Ready
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.{TaskAppPage}
import d2spa.client.MegaContent
import d2spa.client.UpdateQueryProperty
import d2spa.shared.{PropertyMetaInfo, EOValue}
import d2spa.shared.TaskDefine
import d2spa.client.{MegaContent, UpdateEOValueForProperty}

object ERDList {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDList ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDList convert EORef into EOs
  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.finest("ERDList willReceiveProps | currentProps: " + currentProps)
      //log.finest("ERDList willReceiveProps | nextProps: " + nextProps)

      val cEO = currentProps.d2wContext
      val nEO = nextProps.d2wContext
      val eoChanged = !cEO.equals(nEO)

      //D2SpaLogger.logfinest(D2SpaLogger.ALL, "ERDList willReceiveProps | eoChanged: " + eoChanged)


      Callback.when(eoChanged) {
        mounted(nextProps)
      }
    }


    // We need to fire rule for Destination Entity (in case it is not given by the eomodel
    // But be careful, the rule can return nothing and it shouldn't be an error
    def mounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get

      D2SpaLogger.logfinest(entityName, "ERDList mounted " + entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      val ruleResults = p.proxy.value.ruleResults
      val listConfigurationNameRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.listConfigurationName)
      val destinationEntityRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.destinationEntity)
      val rulesPots = List(listConfigurationNameRuleResultPot, destinationEntityRuleResultPot)
      val rules = RuleUtils.firingRulesFromPotFiredRuleResult(rulesPots)
      val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)

      Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))
    }

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName,"ERDList render " + entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      //log.finest("ERDList render with D2WContext: " + d2wContext)

      // to get access to the latest version of the eo we use the previous page context
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value.cache, entityName, pageContext.eo.get)

      eoOpt match {
        case Some(eo) =>
          d2wContext.propertyKey match {
            case Some(propertyName) =>
              val eomodelOpt = p.proxy.value.cache.eomodel

              eomodelOpt match {
                case Ready(eomodel) =>
                  val ruleResults = p.proxy.value.ruleResults

                  val listDestinationEntityOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, RuleKeys.listConfigurationName)

                  listDestinationEntityOpt match {
                    case Some(pageConfiguration) =>


                      val ruleResultsModel = p.proxy.value.ruleResults

                      val listDestinationEntityOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.destinationEntity)
                      D2SpaLogger.logfinest(entityName,": " + listDestinationEntityOpt)

                      val destinationEntityNameOpt = listDestinationEntityOpt match {
                        case Some(aDestinationEntityName) => Some(aDestinationEntityName)
                        case None =>
                          val entity = EOModelUtils.entityNamed(eomodel, entityName).get
                          val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                          destinationEntityOpt match {
                            case Some(destinationEntity) => Some(destinationEntity.name)
                            case None => None
                          }

                      }

                      destinationEntityNameOpt match {
                        case Some(destinationEntityName) =>
                          val listConfigurationNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.listConfigurationName)
                          D2SpaLogger.logfinest(entityName,"ERDList render | listConfigurationNameOpt " + listConfigurationNameOpt)

                          D2SpaLogger.logfinest(entityName,"ERDList render | dataRep " + eo.eo.entityName + " propertyName: " + propertyName + " destinationEntityName: " + destinationEntityName)

                          val embeddedListD2WContext = PageContext (
                            dataRep = Some(DataRep(eosAtKeyPath = Some(EOsAtKeyPath(eo, propertyName, destinationEntityName)))),
                            d2wContext = D2WContext(
                              entityName = Some(destinationEntityName),
                              task = Some(TaskDefine.list),
                              pageConfiguration = Some(pageConfiguration)
                            )
                          )
                          log.finest("ERDList render embedded list with context " + embeddedListD2WContext)
                          <.div(NVListComponent(p.router, embeddedListD2WContext, true, p.proxy))

                        case None => <.div("No destinaton Entity name")
                      }
                    case None =>
                      <.div("page configuration not yet fired")
                  }

                case _ => <.div("no eomodel")
              }
            case _ => <.div("no propertyName")
          }
        case None => <.div("")
      }

    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
