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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  def eoWith(eos: Seq[EO], entity: EOEntity, id: String) = {
    println("eoWith | entity name " + entity.name + " id " + id + " eos " + eos )
    //log.debug("id " + id + " class " + id.getClass.getName)
    if (id.equals("None")) None
    val pk = EOPk(id.split("_").map(_.toInt).toList)

    val optEO = eos.find(eo => {
      pk.equals(eo.pk)
    })
    optEO
  }

  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      log.debug("ERD2WEditToOneRelationship | willReceiveProps")

      val p = nextProps
      val d2wContext = p.d2wContext
      val eomodel = p.proxy.value.cache.eomodel.get
      val cache = p.proxy.value.cache

      val ruleResults = currentProps.proxy.value.ruleResults
      val keyWhenRelationshipRuleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
      val continueMount = keyWhenRelationshipRuleResultOpt match {
        case Some(keyWhenRelationshipRuleResult) =>
          log.debug("ERD2WEditToOneRelationship | willReceiveProps | Key when relathionship: YES ")
          val keyWhenRelationshipOpt = RuleUtils.ruleStringValueWithRuleResult(keyWhenRelationshipRuleResultOpt)
          val keyWhenRelahionship = keyWhenRelationshipOpt.get
          val propertyName = d2wContext.propertyKey.get
          val entityName = p.d2wContext.entityName.get

          val entity = EOModelUtils.entityNamed(eomodel, entityName).get

          val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
          val destinationEntity = destinationEntityOpt.get

          val isHydrated = Hydration.isHydratedForPropertyKeys(
            eomodel,
            cache,
            DrySubstrate(fetchSpecification = Some(EOFetchAll(destinationEntity.name))),
            List(keyWhenRelahionship))
          log.debug("ERD2WEditToOneRelationship | willReceiveProps | key when relathionship but isHydrated " + isHydrated)

          !isHydrated
        case None =>
          log.debug("ERD2WEditToOneRelationship | willReceiveProps | no key when relathionship")
          true
      }

      Callback.when(continueMount) {
        mounted(nextProps)
      }
      //mounted(nextProps)

    }



    def mounted(p: Props) = {
      log.debug("ERD2WEditToOneRelationship mounted")

      val d2wContext = p.d2wContext
      val propertyName = d2wContext.propertyKey.get


      val entityName = p.d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.debug("ERD2WEditToOneRelationship mounted: entity " + entity)
      log.debug("ERD2WEditToOneRelationship mounted: propertyName " + propertyName)

      log.debug("ERD2WEditToOneRelationship mounted: eomodel " + eomodel)
      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.debug("ERD2WEditToOneRelationship mounted: destinationEntity " + destinationEntityOpt)
      destinationEntityOpt match {
        case Some(destinationEntity) =>
          val ruleResults = p.proxy.value.ruleResults
          val keyWhenRelationshipRuleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)

          log.debug("ERD2WEditToOneRelationship mounted | keyWhenRelationshipRuleResultOpt is defined " + keyWhenRelationshipRuleResultOpt.isDefined)

          Callback.when(!keyWhenRelationshipRuleResultOpt.isDefined)(p.proxy.dispatchCB(
              FireActions(
                d2wContext, // rule Container
                List[D2WAction](
                  FireRule(d2wContext, RuleKeys.keyWhenRelationship)
                )
              )
            )) >>
            Callback.when(keyWhenRelationshipRuleResultOpt.isDefined)(p.proxy.dispatchCB(
              FireActions(
                d2wContext, // rule Container
                List[D2WAction](
                  // Already done at Page level
                  // Hydration(DrySubstrate(eo = Some(p.eo)),WateringScope(Some(fireDisplayPropertyKeys))),

                  // Substrate defined by objects to fetch for destination entity name
                  // Watered by keyWhenRelationship which has to to be looked up in the rules (it will be found because of the above rule firing)
                  // Example:
                  //    Hydration(
                  //      DrySubstrate(None,None,Some(FetchSpecification(Customer,None))),
                  //      WateringScope(Some(RuleFault(D2WContextFullFledged(Some(Project),Some(edit),Some(customer),None),keyWhenRelationship)))))

                  Hydration(DrySubstrate(fetchSpecification = Some(EOFetchAll(destinationEntity.name))), WateringScope(PotFiredRuleResult(Right(keyWhenRelationshipRuleResultOpt.get))))

                )
              )
            ))

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
      //log.debug("id " + id + " class " + id.getClass.getName)
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
      log.debug("ERD2WEditToOneRelationship render")
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val eomodel = p.proxy.value.cache.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel, entityName).get
      log.debug("ERD2WEditToOneRelationship render entity " + entity)

      val taskName = d2wContext.task.get
      val ruleResultsModel = p.proxy.value.ruleResults
      log.debug("ERD2WEditToOneRelationship render ruleResultsModel " + ruleResultsModel)


      d2wContext.eo match {
        case Some(deo) =>



          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value.cache, entityName, deo)
          eoOpt match {
            case Some(eo) =>
              val propertyName = d2wContext.propertyKey.get
              //val properyD2WContext = RuleUtils.convertD2WContextToFullFledged(d2wContext(p))

              //log.debug("+ rules " + p.property.ruleResults)
              log.debug("task  " + taskName)

              //log.debug("Edit To One Relationship " + eo)
              val ruleResultsOpt = RuleUtils.ruleContainerForContext(ruleResultsModel, d2wContext)
              ruleResultsOpt match {
                case Some(ruleResults) => {
                  log.debug("ERD2WEditToOneRelationship render propertyMetaInfo rule result" + ruleResults)


                  val keyWhenRelationshipRuleOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)

                  keyWhenRelationshipRuleOpt match {
                    case Some(keyWhenRelationship) => {
                      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                      destinationEntityOpt match {
                        case Some(destinationEntity) =>
                          val eoCache = p.proxy.value.cache.eos

                          log.debug("ERD2WEditToOneRelationship render Look into the cache for objects for entity named " + destinationEntity.name)
                          log.debug("ERD2WEditToOneRelationship render eoCache " + eoCache)
                          val destinationEOs = EOCacheUtils.objectsForEntityNamed(eoCache, destinationEntity.name)

                          <.div(
                            //{
                            //log.debug("p.property.ruleKeyValues " + p.property.ruleKeyValues)
                            /*   <.div("destinationEntity " + p.proxy.value.eomodel.get +  " destinationEOs "),
                             <.div("entity " +entity),
                             <.div("propertyName " +propertyName)*/

                            destinationEOs match {
                              case Some(eos) => {
                                log.debug("ERD2WEditToOneRelationship render eoRefs " + eos)
                                log.debug("ERD2WEditToOneRelationship render eo " + eo)
                                val destinationEO = EOValue.valueForKey(eo, propertyName)
                                log.debug("ERD2WEditToOneRelationship render destinationEO " + destinationEO)
                                val defaultValue = destinationEO match {
                                  case Some(ObjectValue(eoV)) => EOValue.pk(eomodel,eoV) match {
                                    case Some(pk) => pk.toString
                                    case None => "None"
                                  }
                                  case _ => "None"
                                }
                                <.div(
                                  <.select(bss.formControl, ^.value := defaultValue, ^.id := "priority", ^.onChange ==> { e: ReactEventFromInput =>
                                    p.proxy.dispatchCB(UpdateEOValueForProperty(eo, d2wContext, EOValue.objectValue(eoWith(eos, destinationEntity, e.currentTarget.value))))
                                  },
                                    {
                                      val tuples = eos map (deo => {

                                        //log.debug("id " + id + " for eo: " + x)
                                        val displayName = EOValue.stringValueForKey(deo, keyWhenRelationship)
                                        val valueString = deo.pk.pks.mkString("_")
                                        (valueString, displayName)
                                      })
                                      // remove None
                                      val tuplesWithNone = ("None", "- none -") :: tuples
                                      log.debug("valid tuples " + tuples)
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
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))
}
