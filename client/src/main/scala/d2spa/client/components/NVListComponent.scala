package d2spa.client.components

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{Button, CommonStyle}

import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.react.ModelProxy
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.logger.log
import d2spa.client._
import d2spa.shared._
import diode.data.Ready

object NVListComponent {


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent])

  class Backend($ : BackendScope[Props, Unit]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      log.debug("NVListComponent willReceiveProps | currentProps: " + currentProps)
      log.debug("NVListComponent willReceiveProps | nextProps: " + nextProps)
      val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityChanged = !cEntityName.equals(nEntityName)

      val cDataRep = currentProps.d2wContext.dataRep
      val nDataRep = nextProps.d2wContext.dataRep
      val dataRepChanged = !cDataRep.equals(nDataRep)

      val anyChange = entityChanged || dataRepChanged

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }

    def willmounted(p: Props) = {
      //val destinationEntity = EOModelUtilsdes
      log.debug("NVListComponent mounted")
      val eomodel = p.proxy.value.eomodel.get
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      //val propertyName = staleD2WContext.propertyKey.get
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val ruleResultsModel = p.proxy.value.ruleResults

      //val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
      //log.debug("ERDList mounted: dataNotFetched" + dataNotFetched)

      log.debug("NVListComponent mounted: entity: " + entity)
      log.debug("NVListComponent mounted: eomodel: " + eomodel)

      // listConfigurationName
      // Then with D2WContext:
      // - task = 'list'
      // - entity.name = 'Project'
      // - pageConfiguration = <listConfigurationName>
      log.debug("NVListComponent mounted: d2wContext: " + d2wContext)


      val fireDisplayPropertyKeys = FireRule(p.d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("NVListComponent mounted: fireDisplayPropertyKeys: " + fireDisplayPropertyKeys)


      // case class DataRep (fetchSpecification: Option[EOFetchSpecification] = None, eosAtKeyPath: Option[EOsAtKeyPath] = None)

      val dataRep = d2wContext.dataRep

      val fireActions: Option[List[D2WAction]] = dataRep match {
        case Some(DataRep(Some(fetchSpecification), _)) => None
          // TODO Restore it
          /*val drySubstrate = DrySubstrate(fetchSpecification = Some(fetchSpecification))
          val fireListDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)
          val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireListDisplayPropertyKeys)

          Some(
            List(
              fireListConfiguration, // standard FieRule
              fireListDisplayPropertyKeys, // standard FieRule
              Hydration(
                drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                WateringScope(
                  Some(ruleFaultListDisplayPropertyKeys)
                )
              ), // populate with properties to be fired rule
              FireRules(KeysSubstrate(Some(ruleFaultListDisplayPropertyKeys)), d2wContext, RuleKeys.componentName)
            )
          )*/

        case Some(DataRep(_, Some(eosAtKeyPath))) => {
          log.debug("NVListComponent mounted: EOsAtKeyPath: " + eosAtKeyPath)
          val eo = eosAtKeyPath.eo
          val propertyName = eosAtKeyPath.keyPath
          val entity = eo.entity

          val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)

          val fireListDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)
          log.debug("NVListComponent mounted: fireListDisplayPropertyKeys: " + fireListDisplayPropertyKeys)

          val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireListDisplayPropertyKeys)
          log.debug("NVListComponent mounted: ruleFaultListDisplayPropertyKeys: " + ruleFaultListDisplayPropertyKeys)


          val drySubstrate: DrySubstrate = DrySubstrate(Some(EOsAtKeyPath(eo, propertyName)))
          log.debug("NVListComponent mounted: drySubstrate: " + drySubstrate)

          val fas = Some(
            List(
              fireListDisplayPropertyKeys, // standard FieRule
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
              Hydration(
                drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                WateringScope(  // RuleFault
                  Some(ruleFaultListDisplayPropertyKeys)
                )
              ), // populate with properties to be fired rule
              FireRules(KeysSubstrate(Some(ruleFaultListDisplayPropertyKeys)), d2wContext, RuleKeys.componentName)
            )
          )
          log.debug("NVListComponent mounted: fireActions: " + fas)

          fas
        }
        case _ => None
      }

      log.debug("NVListComponent mounted: FireActions: " + fireActions)

      Callback.when(fireActions.isDefined)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          fireActions.get
        )
      ))

    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO(TaskDefine.list, eo)))
    }

    def editEO(eo: EO) = {
      val pk = EOValue.pk(eo)
      val d2wContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.edit), eo = Some(eo))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(RegisterPreviousPage(d2wContext)))
    }

    def deleteEO(eo: EO) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList("list", eo)))
    }

    def render(p: Props) = {
      log.debug("NVListComponent render ")
      val staleD2WContext = p.d2wContext
      val d2wContextOpt: Option[D2WContext] = staleD2WContext.dataRep match {
        case Some(DataRep(_, Some(eosAtKeyPath))) => Some(staleD2WContext)
        case _ =>
          val d2wContextOpt = p.proxy.value.previousPage
          d2wContextOpt match {
            case Some(c) => Some(c)
            case _ => None
          }
      }

      log.debug("NVListComponent render |  proxy d2wContext: " + d2wContextOpt)

      d2wContextOpt match {
        case Some(d2wContext) =>


          val entityName = d2wContext.entityName.get
          log.debug("NVListComponent render for entity: " + entityName)


          val ruleResultsModel = p.proxy.value.ruleResults
          log.debug("NVListComponent render ruleResultsModel: " + ruleResultsModel)

          val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
          log.debug("NVListComponent render task displayPropertyKeys " + displayPropertyKeys)
          val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)


          val dataRepOpt = d2wContext.dataRep

          log.debug("dataRepOpt " + dataRepOpt)
          val eos: List[EO] = dataRepOpt match {
            case Some(dataRep) => {
              val cache = p.proxy.value.cache
              dataRep match {
                case DataRep(Some(fs), _) =>
                  log.debug("NVListCompoennt look for objects in cache with fs " + fs)
                  log.debug("NVListCompoennt look for objects in cache " + cache)
                  EOCacheUtils.objectsWithFetchSpecification(cache, fs)

                case DataRep(_, Some(eosAtKeyPath)) => {
                  log.debug("NVListComponent render eosAtKeyPath " + eosAtKeyPath)
                  val eovalueOpt = EOValue.valueForKey(eosAtKeyPath.eo, eosAtKeyPath.keyPath)
                  eovalueOpt match {
                    case Some(eovalue) =>

                      // ObjectsValue(Vector(1))
                      eovalue match {
                        case ObjectsValue(pks) =>
                          log.debug("NVListComponent render pks " + pks)
                          EOCacheUtils.outOfCacheEOUsingPks(p.proxy.value,entityName,pks).toList
                        case _ => List.empty[EO]
                      }
                    case _ =>
                      List.empty[EO]
                  }
                }
                case _ => List.empty[EO]
              }
            }
            case _ => List.empty[EO]
          }
          log.debug("NVListComponent render eos " + eos)


          val countText = eos.size + " " + (if (entityDisplayNameOpt.isDefined) entityDisplayNameOpt.get else "")

          <.div(
            {
              val eoOnError = eos.find(x => (x.validationError.isDefined))
              if (eoOnError.isDefined) {
                val validationError = eoOnError.get.validationError.get
                <.div(<.span(^.color := "red", ^.dangerouslySetInnerHtml := validationError))
              } else <.div()
            },
            {
              <.table(^.className := "listPage",
                <.tbody(
                  <.tr(^.className := "listHeader",
                    <.td(^.className := "listHeaderEntityName",
                      <.span(^.className := "attribute", countText)
                    ),
                    if (p.isEmbedded) <.td() else <.td(^.className := "listHeaderReturnButton", <.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName))))
                  )
                ),
                <.tbody(
                  <.tr(
                    <.td(
                      <.table(^.className := "listRepetition",
                        <.tbody(
                          <.tr(^.className := "listRepetitionColumnHeader",
                            <.td(), {
                              displayPropertyKeys toTagMod (propertyKey =>
                                <.td(^.className := "listRepetitionColumnHeader", {
                                  val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                                  val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
                                  val displayString = displayNameFound match {
                                    case Some(stringValue) => {
                                      //case Some(stringValue) => {
                                      stringValue
                                    }
                                    case _ => propertyKey
                                  }
                                  <.span(^.className := "listRepetitionColumnHeader", displayString)
                                })
                                )
                            }
                          )
                        ),
                        <.tbody(
                          eos toTagMod (eo =>
                            <.tr(
                              <.td(
                                <.img(^.className := "IconButton", ^.src := "/assets/images/Magglass.gif", ^.onClick --> inspectEO(eo)),
                                <.img(^.className := "IconButton", ^.src := "/assets/images/Write.gif", ^.onClick --> editEO(eo)),
                                <.img(^.className := "IconButton", ^.src := "/assets/images/Clone.gif")
                              ),
                              displayPropertyKeys toTagMod (
                                propertyKey => {
                                  val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                                  <.td(^.className := "list1",
                                    "toto"
                                    //D2WComponentInstaller(p.router, propertyD2WContext, eo, p.proxy)
                                  )

                                }
                                ),
                              if (p.isEmbedded) <.td() else <.td(<.img(^.className := "IconButton", ^.src := "/assets/images/trashcan-btn.gif", ^.onClick --> deleteEO(eo)))
                            )
                            )
                        )
                      )
                    )
                  )
                )
              )
            }
          )
        case _ => <.div("No context")
      }

    }
  }


  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps,scope.nextProps))
    //.componentDidMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, isEmbedded, proxy))


}
