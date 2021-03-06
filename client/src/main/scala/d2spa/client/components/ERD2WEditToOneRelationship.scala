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

sealed trait TodoPriority

case object TodoLow extends TodoPriority

case object TodoNormal extends TodoPriority

case object TodoHigh extends TodoPriority


object ERD2WEditToOneRelationship {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])

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

  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      log.finest("ERD2WEditToOneRelationship | willReceiveProps")

      val p = nextProps
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val eomodel = p.proxy.value.cache.eomodel.get
      val cache = p.proxy.value.cache

      val ruleResults = currentProps.proxy.value.ruleResults
      val keyWhenRelationshipRuleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
      val continueMount = keyWhenRelationshipRuleResultOpt match {
        case Some(keyWhenRelationshipRuleResult) =>
          log.finest("ERD2WEditToOneRelationship | willReceiveProps | Key when relathionship: YES ")
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
          log.finest("ERD2WEditToOneRelationship | willReceiveProps | key when relathionship but isHydrated " + isHydrated)

          !isHydrated
        case None =>
          log.finest("ERD2WEditToOneRelationship | willReceiveProps | no key when relathionship")
          true
      }

      //Callback.when(continueMount) {
        mounted(nextProps)
      //}
      //mounted(nextProps)

    }



    def mounted(p: Props) = {
      log.finest("ERD2WEditToOneRelationship mounted")

      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val propertyName = d2wContext.propertyKey.get


      val entityName = d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.finest("ERD2WEditToOneRelationship mounted: entity " + entity)
      log.finest("ERD2WEditToOneRelationship mounted: propertyName " + propertyName)

      log.finest("ERD2WEditToOneRelationship mounted: eomodel " + eomodel)
      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.finest("ERD2WEditToOneRelationship mounted: destinationEntity " + destinationEntityOpt)
      destinationEntityOpt match {
        case Some(destinationEntity) =>
          val ruleResults = p.proxy.value.ruleResults
          log.finest("ERD2WEditToOneRelationship | mounted | ruleResults " + ruleResults)
          val keyWhenRelationshipRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
          log.finest("ERD2WEditToOneRelationship | mounted | keyWhenRelationshipRuleResultPot " + keyWhenRelationshipRuleResultPot)
          val rulesPots = List(keyWhenRelationshipRuleResultPot)
          log.finest("ERD2WEditToOneRelationship | mounted | rulesPots " + rulesPots)
          val rules = RuleUtils.firingRulesFromPotFiredRuleResult(rulesPots)
          val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)
          log.finest("ERD2WEditToOneRelationship | mounted | ruleRequestOpt " + ruleRequestOpt)


          val eoCache = p.proxy.value.cache
          val destinationEOs = EOCacheUtils.dbEOsForEntityNamed(eoCache, destinationEntity.name)
          log.finest("ERD2WEditToOneRelationship | mounted | destinationEOs " + destinationEOs)

          destinationEOs match {
            case Some(eos) =>
              Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))

            case None =>
              log.finest("ERD2WEditToOneRelationship | mounted | destinationEOs " + destinationEOs)

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


    /*def eoRefWith(eos: Seq[EO], entity: EOEntity, id: String) = {
      //log.finest("id " + id + " class " + id.getClass.getName)
      if (id.equals("None")) None
      val idAsInt = id.toInt
      val pkAttributeName = entity.pkAttributeName
      val optEO = eos.find(eo => {
        val optPk = EOValueUtils.pk(eo)
        optPk.isDefined && optPk.get.equals(idAsInt)
      })
      if (optEO.isDefined) Some(EORef(entity.name, EOValueUtils.pk(optEO.get).get)) else None
    }*/

    def render(p: Props) = {
      log.finest("ERD2WEditToOneRelationship render")
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.finest("ERD2WEditToOneRelationship render entity " + entity)

      val taskName = d2wContext.task.get
      val ruleResultsModel = p.proxy.value.ruleResults
      log.finest("ERD2WEditToOneRelationship render ruleResultsModel " + ruleResultsModel)


      pageContext.eo match {
        case Some(deo) =>
          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value.cache, entityName, deo)
          eoOpt match {
            case Some(eoContaining) =>
              val propertyName = d2wContext.propertyKey.get
              //val properyD2WContext = RuleUtils.convertD2WContextToFullFledged(d2wContext(p))

              //log.finest("+ rules " + p.property.ruleResults)
              log.finest("task  " + taskName)

              //log.finest("Edit To One Relationship " + eoContaining)
              val ruleResultsOpt = RuleUtils.ruleContainerForContext(ruleResultsModel, d2wContext)
              ruleResultsOpt match {
                case Some(ruleResults) => {
                  log.finest("ERD2WEditToOneRelationship render propertyMetaInfo rule result" + ruleResults)


                  val keyWhenRelationshipRuleOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)

                  keyWhenRelationshipRuleOpt match {
                    case Some(keyWhenRelationship) => {
                      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                      destinationEntityOpt match {
                        case Some(destinationEntity) =>
                          val eoCache = p.proxy.value.cache

                          log.finest("ERD2WEditToOneRelationship render Look into the cache for objects for entity named " + destinationEntity.name)
                          log.finest("ERD2WEditToOneRelationship render eoCache " + eoCache)
                          val destinationEOs = EOCacheUtils.dbEOsForEntityNamed(eoCache, destinationEntity.name)

                          <.div(
                            //{
                            //log.finest("p.property.ruleKeyValues " + p.property.ruleKeyValues)
                            /*   <.div("destinationEntity " + p.proxy.value.eomodel.get +  " destinationEOs "),
                             <.div("entity " +entity),
                             <.div("propertyName " +propertyName)*/

                            destinationEOs match {
                              case Some(eos) => {
                                log.finest("ERD2WEditToOneRelationship render eoRefs " + eos)
                                log.finest("ERD2WEditToOneRelationship render eoContaining " + eoContaining)
                                // the selection
                                val destinationEO = eoContaining.valueForKey(propertyName)
                                log.finest("ERD2WEditToOneRelationship render destinationEO " + destinationEO)
                                val defaultValue = destinationEO match {
                                  case Some(ObjectValue(eoPK)) =>
                                    EOValue.juiceEOPkString(eoPK)
                                  case _ => "None"
                                }
                                log.finest("ERD2WEditToOneRelationship render defaultValue " + defaultValue)
                                log.finest("ERD2WEditToOneRelationship | render | keyWhenRelationship " + keyWhenRelationship)

                                <.div(
                                  <.select(bss.formControl, ^.value := defaultValue, ^.id := "priority", ^.onChange ==> { e: ReactEventFromInput =>
                                    p.proxy.dispatchCB(UpdateEOValueForProperty(eoContaining, pageContext, EOValue.objectValue(eoWith(eos, destinationEntity, e.currentTarget.value))))
                                  },
                                    {
                                      log.finest("ERD2WEditToOneRelationship | render | destination eos " + eos)
                                      log.finest("ERD2WEditToOneRelationship | render | destination eos " + eos)
                                      val tuples = eos map (deoContaining => {

                                        //log.finest("id " + id + " for eoContaining: " + x)
                                        val displayName = EOValue.stringValueForKey(deoContaining, keyWhenRelationship)
                                        val deo = deoContaining.eo
                                        val valueString =  EOValue.juiceEOPkString(deo.pk)
                                        (valueString, displayName)
                                      })
                                      // remove None
                                      val tuplesWithNone = ("None", "- none -") :: tuples
                                      log.finest("valid tuples " + tuples)
                                      tuplesWithNone toTagMod (eo => {
                                        <.option(^.value := eo._1, eo._2)
                                      })
                                    }
                                  )
                                )
                              }
                              case _ => {
                                <.div("No destination objects")
                              }
                            }
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


  private val component = ScalaComponent.builder[Props]("ERD2WEditToOneRelationship")
    .renderBackend[Backend]
    //.componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
