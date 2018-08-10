package d2spa.client.components

import d2spa.client.SPAMain.TaskAppPage
import d2spa.client._
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._

object ERD2WInspect {


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  class Backend($: BackendScope[Props, Unit]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.debug("ERD2WInspect willReceiveProps | currentProps: " + currentProps)
      //log.debug("ERD2WInspect willReceiveProps | nextProps: " + nextProps)
      val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityChanged = !cEntityName.equals(nEntityName)

      val cDataRep = currentProps.d2wContext.dataRep
      val nDataRep = nextProps.d2wContext.dataRep
      val dataRepChanged = !cDataRep.equals(nDataRep)

      val cD2WContext = currentProps.d2wContext
      val nD2WContext = nextProps.d2wContext
      val d2wContextChanged = !cD2WContext.equals(nD2WContext)


      val anyChange = entityChanged || dataRepChanged || d2wContextChanged
      //log.debug("ERD2WInspect willReceiveProps | anyChange: " + anyChange)

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }

    def willmounted(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      //val destinationEntity = EOModelUtilsdes
      val pageConfigurationDebug = d2wContext.pageConfiguration match {
        case PotFiredKey(Right(s)) => s
        case PotFiredKey(Left(_)) => "toBeFired"
      }
      D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted for " + d2wContext.entityName + " task " + d2wContext.task + " page configuration " + pageConfigurationDebug)
      val eomodel = p.proxy.value.cache.eomodel.get
      //val d2wContext = p.proxy.value.previousPage.get
      //val propertyName = staleD2WContext.propertyKey.get
      val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
      entityOpt match {
        case Some(entity) =>
          val ruleResultsModel = p.proxy.value.ruleResults

          //val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
          //log.debug("ERDList mounted: dataNotFetched" + dataNotFetched)

          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: entity: " + entity.name)
          //log.debug("NVListComponent mounted: eomodel: " + eomodel)

          // listConfigurationName
          // Then with D2WContext:
          // - task = 'list'
          // - entity.name = 'Project'
          // - pageConfiguration = <listConfigurationName>
          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: d2wContext: " + d2wContext)

          val displayPropertyKeysRuleResultOpt = RuleUtils.ruleResultForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
          // Fire "displayPropertyKeys" ?
          val fireDisplayPropertyKeysOpt = displayPropertyKeysRuleResultOpt match {
            case Some(_) => None
            case None => Some(FireRule(d2wContext, RuleKeys.displayPropertyKeys))
          }
          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: fireDisplayPropertyKeys: " + displayPropertyKeysRuleResultOpt.isDefined)


          // Fire "isEditAllowed" ?
          val fireIsEditAllowedOpt = RuleUtils.potentialFireRule(p.proxy.value.ruleResults, d2wContext, RuleKeys.isEditAllowed)
          val fireIsInspectAllowedOpt = RuleUtils.potentialFireRule(p.proxy.value.ruleResults, d2wContext, RuleKeys.isInspectAllowed)
          val fireDisplayNameForEntityOpt = RuleUtils.potentialFireRule(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayNameForEntity)
          val fireComponentNamesOpt = RuleUtils.potentialFireRules(p.proxy.value.ruleResults, d2wContext, displayPropertyKeysRuleResultOpt, RuleKeys.componentName)
          val fireDisplayNameForPropertiesOpt = RuleUtils.potentialFireRules(p.proxy.value.ruleResults, d2wContext, displayPropertyKeysRuleResultOpt, RuleKeys.displayNameForProperty)


          val dataRep = d2wContext.dataRep
          val drySubstrateOpt: Option[DrySubstrate] = dataRep match {
            case Some(DataRep(Some(fetchSpecification), _)) =>
              Some(DrySubstrate(fetchSpecification = Some(fetchSpecification)))

            case Some(DataRep(_, Some(eosAtKeyPath))) => {
              Some(DrySubstrate(Some(eosAtKeyPath)))
            }
            case _ => None
          }
          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: drySubstrateOpt: " + drySubstrateOpt)


          // TODO Hydration should be avoided if the objects are already hydrated
          val hydrationOpt = drySubstrateOpt match {
            case Some(drySubstrate) =>
              displayPropertyKeysRuleResultOpt match {
                case Some(displayPropertyKeysRuleResult) =>
                  val displayPropertyKeys = RuleUtils.ruleListValueWithRuleResult(displayPropertyKeysRuleResultOpt)

                  val isHydrated = Hydration.isHydratedForPropertyKeys(p.proxy.value.cache.eomodel.get, p.proxy.value.cache, drySubstrate, displayPropertyKeys)
                  //val isHydrated = false
                  if (isHydrated) {
                    None
                  } else {
                    Some(Hydration(
                      drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                      WateringScope(
                        ruleResult = PotFiredRuleResult(value = Right(displayPropertyKeysRuleResult))
                      )))
                  }
                case None =>
                  D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: drySubstrate: " + drySubstrate)
                  val fireDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)

                  Some(Hydration(
                    drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                    WateringScope( // RuleFault
                      ruleResult = PotFiredRuleResult(Left(fireDisplayPropertyKeys))
                    )))
              }

            case None =>
              None
          }


          val fireActions: List[Option[D2WAction]] = List(
            fireDisplayPropertyKeysOpt, // standard Fire Rule
            fireIsEditAllowedOpt, // standard Fire Rule
            fireIsInspectAllowedOpt, // standard Fire Rule
            fireDisplayNameForPropertiesOpt,
            fireDisplayNameForEntityOpt,
            // Hydrate has 2 parts
            // 1) which eos
            // 2) which propertyKeys
            // Example:
            // - ToOne:
            //   1) all destination eos or restricted (entity, qualifier -> fetchSpecification)
            //   2) one property, the keyWhenRelationship (fireRule is used)
            // Remark: How to get the necessary eos ? Fetch Spec name from rule -> rule. Then use the fetch spec in memory on the cache of eos ?
            //         or current solutions which stores the eos like a pseudo rule result (with property rule storage),
            //         First solution seems better. Fetch spec is stored in the eomodel
            //         fetchSpecificationName or fetchAll is not specified + eomodel fetch spec + cache => eos
            // - ERDList:
            //   1) property eos as eorefs (entity, In qualifier)
            //   2) displayPropertyKeys (fireRule is used)
            // Remark: How to get the necessary eos ? propertyKeyValues (eoref) + cache => eos
            hydrationOpt,
            fireComponentNamesOpt
          )


          val actions = fireActions.flatten
          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: FireActions: " + actions.size)

          Callback.when(!actions.isEmpty)(p.proxy.dispatchCB(
            FireActions(
              d2wContext,
              actions
            )
          ))
        case _ =>
          D2SpaLogger.logDebug(entityName,"ERD2WInspect mounted: no entity for entity name: " + entityName)
          Callback.empty
      }

    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO(TaskDefine.list, eo, false)))
    }

    def editEO(eo: EO) = {
      //val pk = EOValue.pk(eo)
      val d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.edit), eo = Some(eo))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(RegisterPreviousPage(d2wContext)))
    }

    def deleteEO(eo: EO) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList(eo)))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logDebug(entityName,"ERD2WInspect render for entity: " + entityName)

      d2wContext.pageConfiguration match {
        case PotFiredKey(Right(_)) =>
          val ruleResultsModel = p.proxy.value.ruleResults
          //log.debug("ERD2WInspect render ruleResultsModel: " + ruleResultsModel)
          D2SpaLogger.logDebug(entityName,"ERD2WInspect render |  " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)


          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          D2SpaLogger.logDebug(entityName,"ERD2WInspect render task displayPropertyKeys " + displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

          val isInspectAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isInspectAllowed)
          val isEditAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isEditAllowed)
          val cloneAllowed = false && isEditAllowed // not yet implemented
          val showFirstCell = isInspectAllowed || isEditAllowed || cloneAllowed

          D2SpaLogger.logDebug(entityName,"ERD2WInspect render | Inspect: " + isInspectAllowed + " Edit: " + isEditAllowed + " Clone: " + cloneAllowed)

          val dataRepOpt = d2wContext.dataRep

          D2SpaLogger.logDebug(entityName,"dataRepOpt " + dataRepOpt)
          val eoOpt: Option[EO] = dataRepOpt match {
            case Some(dataRep) => {
              val cache = p.proxy.value.cache
              dataRep match {
                case DataRep(Some(fs), _) =>
                  //log.debug("NVListCompoennt look for objects in cache with fs " + fs)
                  //log.debug("NVListCompoennt look for objects in cache " + cache)
                  //D2SpaLogger.logDebug(entityName,"ERD2WInspect look for objects in cache with fs")
                  //EOCacheUtils.objectsFromAllCachesWithFetchSpecification(cache, fs)
                  None

                case DataRep(_, Some(eosAtKeyPath)) => {
                  //log.debug("ERD2WInspect render eosAtKeyPath " + eosAtKeyPath)
                  D2SpaLogger.logDebug(entityName,"ERD2WInspect render eosAtKeyPath " + eosAtKeyPath)
                  val eovalueOpt = EOValue.valueForKey(eosAtKeyPath.eo, eosAtKeyPath.keyPath)
                  D2SpaLogger.logDebug(entityName,"ERD2WInspect render eosAtKeyPath eovalueOpt: " + eovalueOpt)

                  eovalueOpt match {
                    case Some(eovalue) =>

                      // ObjectsValue(Vector(1))
                      eovalue match {
                        case ObjectValue(eo) =>
                          D2SpaLogger.logDebug(entityName,"ERD2WInspect render eo found")
                          EOCacheUtils.outOfCacheEOUsingPk(p.proxy.value.cache, entityName, eo.pk)
                        case _ => None
                      }
                    case _ =>
                      None
                  }
                }
                case _ => None
              }
            }
            case _ => None
          }
          D2SpaLogger.logDebug(entityName,"ERD2WInspect render eo foudn " + eoOpt.isDefined)

          eoOpt match {
            case Some(eo) =>
              val eoRighD2WContext = d2wContext.copy(eo = eoOpt)
              D2SpaLogger.logDebug(entityName,"ERD2WInspect render | d2w context for repetition: " + eoRighD2WContext.entityName + " task " + eoRighD2WContext.task + " propertyKey " + eoRighD2WContext.propertyKey + " page configuration " + eoRighD2WContext.pageConfiguration)

              <.div(PageRepetition(p.router, eoRighD2WContext, p.proxy))

            case None =>
              <.div("EO not found")
          }

        case PotFiredKey(Left(_)) =>
          <.div("page configuration not yet fired")
      }


    }

  }

  private val component = ScalaComponent.builder[Props]("ERD2WInspect")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
