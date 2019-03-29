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


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])

  class Backend($: BackendScope[Props, Unit]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.finest("ERD2WInspect willReceiveProps | currentProps: " + currentProps)
      //log.finest("ERD2WInspect willReceiveProps | nextProps: " + nextProps)
      val cEntityName = currentProps.d2wContext.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.d2wContext.entityName
      val entityChanged = !cEntityName.equals(nEntityName)

      val cDataRep = currentProps.d2wContext.dataRep
      val nDataRep = nextProps.d2wContext.dataRep
      val dataRepChanged = !cDataRep.equals(nDataRep)

      val cD2WContext = currentProps.d2wContext
      val nD2WContext = nextProps.d2wContext
      val d2wContextChanged = !cD2WContext.equals(nD2WContext)


      val anyChange = entityChanged || dataRepChanged || d2wContextChanged
      //log.finest("ERD2WInspect willReceiveProps | anyChange: " + anyChange)

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }

    def willmounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      //val destinationEntity = EOModelUtilsdes
      D2SpaLogger.logfinest(entityName,"ERD2WInspect mounted for " + d2wContext.entityName + " task " + d2wContext.task + " page configuration " + d2wContext.pageConfiguration)
      val eomodel = p.proxy.value.cache.eomodel.get
      //val d2wContext = p.proxy.value.previousPage.get
      //val propertyName = staleD2WContext.propertyKey.get
      val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
      entityOpt match {
        case Some(entity) =>
          D2SpaLogger.logfinest(entityName,"ERD2WInspect mounted: entity: " + entity.name)
          val ruleResults = p.proxy.value.ruleResults

          val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
          val isEditAllowedRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.isEditAllowed)
          val isInspectAllowedRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.isInspectAllowed)
          val metaDataRules = RuleUtils.metaDataFiringRules(p.proxy.value.ruleResults, d2wContext)

          val additionalRulesPots = List(isEditAllowedRuleResultPot, isInspectAllowedRuleResultPot)
          val additionalRules = RuleUtils.firingRulesFromPotFiredRuleResult(additionalRulesPots)
          val rules = metaDataRules ::: additionalRules

          val ruleRequestOpt =  if (rules.isEmpty) {
            None
          } else {
            Some(RuleRequest(d2wContext, rules))
          }
          val dataRep = pageContext.dataRep
          val drySubstrateOpt: Option[DrySubstrate] = dataRep match {
            case Some(DataRep(Some(fetchSpecification), _)) =>
            Some(DrySubstrate(fetchSpecification = Some(fetchSpecification)))

            case Some(DataRep(_, Some(eosAtKeyPath))) => {
              val eoValueOpt = EOValue.valueForKey(eosAtKeyPath.eoContaining, eosAtKeyPath.keyPath)
              val eo = EOValue.juiceEO(eoValueOpt.get).get
              val eoFault = EOFault(eosAtKeyPath.destinationEntityName,eo)
              Some(DrySubstrate(eo = Some(eoFault)))
            }
            case _ => None
          }
          D2SpaLogger.logfinest(entityName,"ERD2WInspect mounted: drySubstrateOpt: " + drySubstrateOpt)

          if (drySubstrateOpt.isDefined) {
            val hydration = Hydration(
              drySubstrateOpt.get, // Hydration of objects at the end of relationship, stored in cache
              WateringScope(ruleResult = displayPropertyKeysRuleResultPot)
            )
            p.proxy.dispatchCB(HydrationRequest(hydration, ruleRequestOpt))
          } else {
            Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))
          }
        case _ =>
          D2SpaLogger.logfinest(entityName,"ERD2WInspect mounted: no entity for entity name: " + entityName)
          Callback.empty
      }
    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eo: EOContaining) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO(TaskDefine.list, eo)))
    }

    def editEO(eoContaining: EOContaining) = {
      //val pk = EOValue.pk(eo)
      val eo = eoContaining.eo
      val d2wContext = PageContext( d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.edit)), eo = Some(eoContaining))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(RegisterPreviousPage(d2wContext)))
    }

    def deleteEO(eo: EOContaining) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList(eo)))
    }

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName,"ERD2WInspect render for entity: " + entityName)

          val ruleResultsModel = p.proxy.value.ruleResults
          //log.finest("ERD2WInspect render ruleResultsModel: " + ruleResultsModel)
          D2SpaLogger.logfinest(entityName,"ERD2WInspect render |  " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)


          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          D2SpaLogger.logfinest(entityName,"ERD2WInspect render task displayPropertyKeys " + displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

          val isInspectAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isInspectAllowed)
          val isEditAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isEditAllowed)
          val cloneAllowed = false && isEditAllowed // not yet implemented
          val showFirstCell = isInspectAllowed || isEditAllowed || cloneAllowed

          D2SpaLogger.logfinest(entityName,"ERD2WInspect render | Inspect: " + isInspectAllowed + " Edit: " + isEditAllowed + " Clone: " + cloneAllowed)

          val dataRepOpt = pageContext.dataRep

          D2SpaLogger.logfinest(entityName,"dataRepOpt " + dataRepOpt)
          val eoOpt: Option[EOContaining] = dataRepOpt match {
            case Some(dataRep) => {
              val cache = p.proxy.value.cache
              dataRep match {
                case DataRep(Some(fs), _) =>
                  //log.finest("NVListCompoennt look for objects in cache with fs " + fs)
                  //log.finest("NVListCompoennt look for objects in cache " + cache)
                  //D2SpaLogger.logfinest(entityName,"ERD2WInspect look for objects in cache with fs")
                  //EOCacheUtils.objectsFromAllCachesWithFetchSpecification(cache, fs)
                  None

                case DataRep(_, Some(eosAtKeyPath)) => {
                  //log.finest("ERD2WInspect render eosAtKeyPath " + eosAtKeyPath)
                  D2SpaLogger.logfinest(entityName,"ERD2WInspect render eosAtKeyPath " + eosAtKeyPath)
                  val eovalueOpt = EOValue.valueForKey(eosAtKeyPath.eoContaining, eosAtKeyPath.keyPath)
                  D2SpaLogger.logfinest(entityName,"ERD2WInspect render eosAtKeyPath eovalueOpt: " + eovalueOpt)

                  eovalueOpt match {
                    case Some(eovalue) =>

                      // ObjectsValue(Vector(1))
                      eovalue match {
                        case ObjectValue(eo) =>
                          D2SpaLogger.logfinest(entityName,"ERD2WInspect render eo found")
                          EOCacheUtils.outOfCacheEOUsingPk(p.proxy.value.cache, entityName, eo)
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
          D2SpaLogger.logfinest(entityName,"ERD2WInspect render eo foudn " + eoOpt.isDefined)

          eoOpt match {
            case Some(eo) =>
              val pageContextEO = pageContext.copy(eo = eoOpt)
              val d2wContext = pageContext.d2wContext
              D2SpaLogger.logfinest(entityName,"ERD2WInspect render | d2w context for repetition: " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

              <.div(PageRepetition(p.router, pageContextEO, p.proxy))

            case None =>
              <.div("EO not found")
          }



    }

  }

  private val component = ScalaComponent.builder[Props]("ERD2WInspect")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
