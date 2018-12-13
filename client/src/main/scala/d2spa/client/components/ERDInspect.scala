package d2spa.client.components

import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.client.{FireRule, _}
import d2spa.shared._
import diode.data.Ready
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.TaskDefine

object ERDInspect {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDInspect ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDInspect convert EORef into EOs
  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.finest("ERDInspect willReceiveProps | currentProps: " + currentProps)
      //log.finest("ERDInspect willReceiveProps | nextProps: " + nextProps)

      val cEO = currentProps.d2wContext.eo
      val nEO = nextProps.d2wContext.eo
      val eoChanged = !cEO.equals(nEO)

      //log.finest("ERDInspect willReceiveProps | eoChanged: " + eoChanged)


      Callback.when(eoChanged) {
        mounted(nextProps)
      }
    }


    // We need to fire rule for Destination Entity (in case it is not given by the eomodel
    // But be careful, the rule can return nothing and it shouldn't be an error
    def mounted(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName,"ERDInspect mounted " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)
      val ruleResultsModel = p.proxy.value.ruleResults

      val embeddedConfigurationNameOpt = RuleUtils.potentialFireRule(ruleResultsModel, d2wContext, RuleKeys.inspectConfigurationName)
      val destinationEntityOpt = RuleUtils.potentialFireRule(ruleResultsModel, d2wContext, RuleKeys.destinationEntity)

      val fireActions =
        List(
          embeddedConfigurationNameOpt, // standard FieRule
          destinationEntityOpt // standard FieRule
        ).flatten


      Callback.when(!fireActions.isEmpty)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          fireActions
        )
      ))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName,"ERDInspect render " + entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      //log.finest("ERDInspect render with D2WContext: " + d2wContext)

      // to get access to the latest version of the eo we use the previous page context
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value.cache, entityName, d2wContext.eo.get)
      D2SpaLogger.logfinest(entityName,"ERDInspect render eoOpt " + eoOpt)

      eoOpt match {
        case Some(eo) =>
          d2wContext.propertyKey match {
            case Some(propertyName) =>
              val eomodelOpt = p.proxy.value.cache.eomodel

              eomodelOpt match {
                case Ready(eomodel) =>
                  d2wContext.pageConfiguration match {
                    case PotFiredKey(Right(_)) =>


                      val ruleResultsModel = p.proxy.value.ruleResults

                      val destinationEntityOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.destinationEntity)
                      D2SpaLogger.logfinest(entityName,"ERDInspect destination entity: " + destinationEntityOpt)

                      val destinationEntityNameOpt = destinationEntityOpt match {
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
                          val configurationNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.inspectConfigurationName)
                          D2SpaLogger.logfinest(entityName,"ERDInspect render | ConfigurationNameOpt " + configurationNameOpt)

                          val potPageConf = configurationNameOpt match {
                            case Some(_) =>
                              PotFiredKey(Right(configurationNameOpt))
                            case None =>
                              val fireEmbeddedConfiguration = FireRule(d2wContext, RuleKeys.inspectConfigurationName)
                              PotFiredKey(Left(fireEmbeddedConfiguration))
                          }


                          //val eoValueOpt = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None

                          //val size = eoValueOpt match {
                          //  case Some(ObjectsValue(eos)) => eos.size
                          //  case _ => 0
                          //}
                          //val size = 1


                          // D2WContext with
                          // - Entity (destinationEntity)
                          // - task = inspect
                          // - DataRep
                          // (the rest is None: previousTask, eo, queryValues, propertyKey, pageConfiguration)
                          val embeddedD2WContext = D2WContext(
                            entityName = Some(destinationEntityName),
                            task = Some(TaskDefine.inspect),
                            dataRep = Some(DataRep(eosAtKeyPath = Some(EOsAtKeyPath(eo, propertyName, destinationEntityName)))),
                            pageConfiguration = potPageConf
                          )
                          //log.finest("ERDInspect render embedded list with context " + embeddedListD2WContext)
                          <.div(ERD2WInspect(p.router, embeddedD2WContext, p.proxy))
                          //<.div("Let's wait until we can " + embeddedD2WContext)
                        case None => <.div("No destinaton Entity name")
                      }
                    case PotFiredKey(Left(_)) =>
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

  private val component = ScalaComponent.builder[Props]("ERDInspect")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
