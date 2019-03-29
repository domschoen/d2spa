package d2spa.client.components

import d2spa.client._
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
import d2spa.client.components.Bootstrap._
import d2spa.client.components.GlobalStyles

import scalacss.ScalaCssReact._
import d2spa.client.SPAMain.TaskAppPage
import diode.data.Ready
import d2spa.client.logger._
import d2spa.shared.EOModelUtils



object ERDEditRelationship {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])
  case class State(selectedEO: Option[EOContaining])

  def eoWith(eos: Seq[EOContaining], entity: EOEntity, id: String) = {
    println("eoWith | entity name " + entity.name + " id " + id + " eos " + eos )
    //log.finest("id " + id + " class " + id.getClass.getName)
    if (id.equals("None")) None
    val pk = EOPk(id.split("_").map(_.toInt).toList)

    val optEO = eos.find(eoContaining => {
      val eo = eoContaining.eo
      pk.equals(eo.pk)
    })
    optEO
  }

  class Backend($: BackendScope[Props, State]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      log.finest("ERDEditRelationship | willReceiveProps")

      val p = nextProps
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val eomodel = p.proxy.value.cache.eomodel.get
      val cache = p.proxy.value.cache

      val ruleResults = currentProps.proxy.value.ruleResults
      val keyWhenRelationshipRuleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
      val continueMount = keyWhenRelationshipRuleResultOpt match {
        case Some(keyWhenRelationshipRuleResult) =>
          log.finest("ERDEditRelationship | willReceiveProps | Key when relathionship: YES ")
          val keyWhenRelationshipOpt = RuleUtils.ruleStringValueWithRuleResult(keyWhenRelationshipRuleResultOpt)
          val keyWhenRelahionship = keyWhenRelationshipOpt.get
          val propertyName = d2wContext.propertyKey.get
          val entityName = d2wContext.entityName.get

          val entity = EOModelUtils.entityNamed(eomodel, entityName).get

          val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
          val destinationEntity = destinationEntityOpt.get

          val isHydrated = HydrationUtils.isHydratedForPropertyKeys(
            eomodel,
            cache,
            DrySubstrate(fetchSpecification = Some(EOFetchAll(destinationEntity.name))),
            List(keyWhenRelahionship))
          log.finest("ERDEditRelationship | willReceiveProps | key when relathionship but isHydrated " + isHydrated)

          !isHydrated
        case None =>
          log.finest("ERDEditRelationship | willReceiveProps | no key when relathionship")
          true
      }

      //Callback.when(continueMount) {
        mounted(nextProps)
      //}
      //mounted(nextProps)

    }



    def mounted(p: Props) = {
      log.finest("ERDEditRelationship mounted")

      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val propertyName = d2wContext.propertyKey.get


      val entityName = d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.finest("ERDEditRelationship mounted: entity " + entity)
      log.finest("ERDEditRelationship mounted: propertyName " + propertyName)

      log.finest("ERDEditRelationship mounted: eomodel " + eomodel)
      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.finest("ERDEditRelationship mounted: destinationEntity " + destinationEntityOpt)
      destinationEntityOpt match {
        case Some(destinationEntity) =>
          val ruleResults = p.proxy.value.ruleResults
          log.finest("ERDEditRelationship | mounted | ruleResults " + ruleResults)
          val keyWhenRelationshipRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
          log.finest("ERDEditRelationship | mounted | keyWhenRelationshipRuleResultPot " + keyWhenRelationshipRuleResultPot)
          val rulesPots = List(keyWhenRelationshipRuleResultPot)
          log.finest("ERDEditRelationship | mounted | rulesPots " + rulesPots)
          val rules = RuleUtils.firingRulesFromPotFiredRuleResult(rulesPots)
          val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)
          log.finest("ERDEditRelationship | mounted | ruleRequestOpt " + ruleRequestOpt)


          val eoCache = p.proxy.value.cache
          val destinationEOs = EOCacheUtils.dbEOsForEntityNamed(eoCache, destinationEntity.name)
          log.finest("ERDEditRelationship | mounted | destinationEOs " + destinationEOs)

          destinationEOs match {
            case Some(eos) =>
              Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))

            case None =>
              log.finest("ERDEditRelationship | mounted | destinationEOs " + destinationEOs)

              val dataRep = pageContext.dataRep
              val drySubstrateOpt: Option[DrySubstrate] = HydrationUtils.drySubstrateFromDataRep(dataRep)
              val hydration = Hydration(DrySubstrate(fetchSpecification = Some(EOFetchAll(destinationEntity.name))), WateringScope(keyWhenRelationshipRuleResultPot))
              p.proxy.dispatchCB(HydrationRequest(hydration, ruleRequestOpt))
          }

        case None =>
          Callback.empty
      }

    }


    def updateValue(e: ReactEventFromInput, p: Props) = {
      // update TodoItem priority
      val newPri = e.currentTarget.value match {
        case p if p == TodoHigh.toString => TodoHigh
        case p if p == TodoNormal.toString => TodoNormal
        case p if p == TodoLow.toString => TodoLow
      }
    }

    def handleSubmit(p: Props, s: State, pageContext: PageContext, ceo: EOContaining, propertyName: String, e: ReactEventFromInput): Callback = {

      e.preventDefaultCB >> {
        log.finest("ERDEditRelationship handleSubmit " + s.selectedEO)
        s.selectedEO match {
          case Some(eoContaining) =>
            val destinationEOsValueOpt = EOValue.valueForKey(eoContaining, propertyName)
            log.finest("ERDEditRelationship handleSubmit : " + destinationEOsValueOpt +  " property " + propertyName)
            val eoPkList: List[EOPk] = destinationEOsValueOpt match {
              case Some(ObjectsValue(eoPks)) =>
                eoPks
              case _ => List.empty[EOPk]
            }
            val eo = eoContaining.eo
            val newEOPkList = eo.pk :: eoPkList
            p.proxy.dispatchCB(UpdateEOValueForProperty(eoContaining, pageContext, ObjectsValue(newEOPkList)))

          case None =>
            Callback.empty
        }
      }
    }
    def handleSelectionChange(eoOpt: Option[EOContaining]) = {
        $.modState(_.copy(selectedEO = eoOpt))
    }

    def render(p: Props, s: State) = {
      log.finest("ERDEditRelationship render")
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.finest("ERDEditRelationship render entity " + entity)

      val taskName = d2wContext.task.get
      val ruleResultsModel = p.proxy.value.ruleResults
      log.finest("ERDEditRelationship render ruleResultsModel " + ruleResultsModel)


      pageContext.eo match {
        case Some(deo) =>
          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value.cache, entityName, deo)
          eoOpt match {
            case Some(eo) =>
              val propertyName = d2wContext.propertyKey.get
              //val properyD2WContext = RuleUtils.convertD2WContextToFullFledged(d2wContext(p))

              //log.finest("+ rules " + p.property.ruleResults)
              log.finest("task  " + taskName)

              //log.finest("Edit To One Relationship " + eo)
              val ruleResultsOpt = RuleUtils.ruleContainerForContext(ruleResultsModel, d2wContext)
              ruleResultsOpt match {
                case Some(ruleResults) => {
                  log.finest("ERDEditRelationship render propertyMetaInfo rule result" + ruleResults)


                  val keyWhenRelationshipRuleOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)

                  keyWhenRelationshipRuleOpt match {
                    case Some(keyWhenRelationship) => {
                      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                      destinationEntityOpt match {
                        case Some(destinationEntity) =>
                          val eoCache = p.proxy.value.cache

                          log.finest("ERDEditRelationship render Look into the cache for objects for entity named " + destinationEntity.name)
                          log.finest("ERDEditRelationship render eoCache " + eoCache)
                          val possibleEOs = EOCacheUtils.dbEOsForEntityNamed(eoCache, destinationEntity.name)

                          <.div(
                            //{
                            //log.finest("p.property.ruleKeyValues " + p.property.ruleKeyValues)
                            /*   <.div("destinationEntity " + p.proxy.value.eomodel.get +  " possibleEOs "),
                             <.div("entity " +entity),
                             <.div("propertyName " +propertyName)*/

                            possibleEOs match {
                              case Some(eos) => {
                                log.finest("ERDEditRelationship render eoRefs " + eos)
                                log.finest("ERDEditRelationship render eo " + eo)
                                val defaultValue = s.selectedEO match {
                                  case Some(eoContaining) =>
                                    val eo = eoContaining.eo
                                    EOValue.juiceEOPkString(eo.pk)
                                  case _ => "None"
                                }
                                log.finest("ERDEditRelationship render defaultValue " + defaultValue)
                                log.finest("ERDEditRelationship | render | keyWhenRelationship " + keyWhenRelationship)


                                //{ e: ReactEventFromInput =>
                                //  p.proxy.dispatchCB(UpdateEOValueForProperty(eo, pageContext, EOValue.objectValue(eoWith(eos, destinationEntity, e.currentTarget.value))))
                                <.form(^.onSubmit ==> { e: ReactEventFromInput => handleSubmit(p, s, pageContext, eo, propertyName, e)},
                                  <.select(bss.formControl, ^.value := defaultValue, ^.id := "priority", ^.onChange ==> {
                                    e: ReactEventFromInput => handleSelectionChange(eoWith(eos, destinationEntity, e.currentTarget.value))
                                  },
                                    {
                                      log.finest("ERDEditRelationship | render | destination eos " + eos)
                                      log.finest("ERDEditRelationship | render | destination eos " + eos)
                                      val tuples = eos map (deo => {

                                        //log.finest("id " + id + " for eo: " + x)
                                        val displayName = EOValue.stringValueForKey(deo, keyWhenRelationship)
                                        val valueString =  EOValue.juiceEOPkString(deo.eo.pk)
                                        (valueString, displayName)
                                      })
                                      // remove None
                                      val tuplesWithNone = ("None", "- none -") :: tuples
                                      log.finest("valid tuples " + tuples)
                                      tuplesWithNone toTagMod (eo => {
                                        <.option(^.value := eo._1, eo._2)
                                      })
                                    }
                                  ),
                                  <.input.submit(^.value := "Add")
                                )
                              }
                              case _ => {
                                <.div("No destination objects")
                              }
                            },
                            ERDList(p.router, pageContext, p.proxy)
                          )
                        case None =>
                          <.div("No destination entity")
                      }
                    }
                    case _ => <.div("keyWhenRelationshipRule is None")
                  }
                }
                case _ => <.div("No rule results")
              }
            case _ => <.div("No eo")
          }
        case _ => <.div("No D2WContext eo")
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("ERDEditRelationship")
    .initialState(State(None))
    .renderBackend[Backend]
    //.componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
