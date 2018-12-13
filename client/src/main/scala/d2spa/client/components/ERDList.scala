package d2spa.client.components

import d2spa.client.{FireRule, _}
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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

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
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get

      D2SpaLogger.logfinest(entityName, "ERDList mounted " + entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)
      val ruleResultsModel = p.proxy.value.ruleResults

      val listConfigurationNameOpt = RuleUtils.potentialFireRule(ruleResultsModel, d2wContext, RuleKeys.listConfigurationName)
      val destinationEntityOpt = RuleUtils.potentialFireRule(ruleResultsModel, d2wContext, RuleKeys.destinationEntity)

      val fireActions =
        List(
          listConfigurationNameOpt, // standard FieRule
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
      D2SpaLogger.logfinest(entityName,"ERDList render " + entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      //log.finest("ERDList render with D2WContext: " + d2wContext)

      // to get access to the latest version of the eo we use the previous page context
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value.cache, entityName, d2wContext.eo.get)

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

                          val potPageConf = listConfigurationNameOpt match {
                            case Some(_) =>
                              PotFiredKey(Right(listConfigurationNameOpt))
                            case None =>
                              val fireListConfiguration = FireRule(d2wContext, RuleKeys.listConfigurationName)
                              PotFiredKey(Left(fireListConfiguration))
                          }


                          //val eoValueOpt = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None

                          //val size = eoValueOpt match {
                          //  case Some(ObjectsValue(eos)) => eos.size
                          //  case _ => 0
                          //}
                          //val size = 1


                          // D2WContext with
                          // - Entity (destinationEntity)
                          // - task = list
                          // - DataRep
                          // (the rest is None: previousTask, eo, queryValues, propertyKey, pageConfiguration)
                          D2SpaLogger.logfinest(entityName,"ERDList render | dataRep " + eo.entityName + " propertyName: " + propertyName + " destinationEntityName: " + destinationEntityName)

                          val embeddedListD2WContext = D2WContext(
                            entityName = Some(destinationEntityName),
                            task = Some(TaskDefine.list),
                            dataRep = Some(DataRep(eosAtKeyPath = Some(EOsAtKeyPath(eo, propertyName, destinationEntityName)))),
                            pageConfiguration = potPageConf
                          )
                          //log.finest("ERDList render embedded list with context " + embeddedListD2WContext)
                          <.div(NVListComponent(p.router, embeddedListD2WContext, true, p.proxy))

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

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))

}
