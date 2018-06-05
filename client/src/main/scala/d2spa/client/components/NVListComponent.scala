package d2spa.client.components

import d2spa.client.RuleUtils.ruleListValueWithRuleResult
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

  class Backend($: BackendScope[Props, Unit]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.debug("NVListComponent willReceiveProps | currentProps: " + currentProps)
      //log.debug("NVListComponent willReceiveProps | nextProps: " + nextProps)
      val cEntityName = currentProps.d2wContext.entityName
      val nEntityName = nextProps.d2wContext.entityName
      val entityChanged = !cEntityName.equals(nEntityName)

      val cDataRep = currentProps.d2wContext.dataRep
      val nDataRep = nextProps.d2wContext.dataRep
      val dataRepChanged = !cDataRep.equals(nDataRep)

      val cEO = currentProps.d2wContext.eo
      val nEO = nextProps.d2wContext.eo
      val eoChanged = !cEO.equals(nEO)


      val anyChange = entityChanged || dataRepChanged || eoChanged
      log.debug("NVListComponent willReceiveProps | anyChange: " + anyChange)

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }

    def willmounted(p: Props) = {
      //val destinationEntity = EOModelUtilsdes
      log.debug("NVListComponent mounted")
      val eomodel = p.proxy.value.eomodel.get
      //val d2wContext = p.proxy.value.previousPage.get
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      //val propertyName = staleD2WContext.propertyKey.get
      val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
      entityOpt match {
        case Some(entity) =>
          val ruleResultsModel = p.proxy.value.ruleResults

          //val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
          //log.debug("ERDList mounted: dataNotFetched" + dataNotFetched)

          log.debug("NVListComponent mounted: entity: " + entity.name)
          //log.debug("NVListComponent mounted: eomodel: " + eomodel)

          // listConfigurationName
          // Then with D2WContext:
          // - task = 'list'
          // - entity.name = 'Project'
          // - pageConfiguration = <listConfigurationName>
          //log.debug("NVListComponent mounted: d2wContext: " + d2wContext)

          val displayPropertyKeysRuleResultOpt = RuleUtils.ruleResultForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
          val fireDisplayPropertyKeysOpt = if (displayPropertyKeysRuleResultOpt.isDefined) None else Some(FireRule(d2wContext, RuleKeys.displayPropertyKeys))
          log.debug("NVListComponent mounted: fireDisplayPropertyKeys: " + displayPropertyKeysRuleResultOpt.isDefined)

          val fireIsEditAllowedRuleResultOpt = RuleUtils.ruleResultForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.isEditAllowed)
          val fireIsEditAllowedOpt = if (fireIsEditAllowedRuleResultOpt.isDefined) None else Some(FireRule(d2wContext, RuleKeys.isEditAllowed))



          // case class DataRep (fetchSpecification: Option[EOFetchSpecification] = None, eosAtKeyPath: Option[EOsAtKeyPath] = None)

          val dataRep = d2wContext.dataRep

          val fireComponentNamesOpt = displayPropertyKeysRuleResultOpt match {
            case Some(displayPropertyKeysRuleResult) =>
              Some(FireRules(KeysSubstrate(ruleResult = Some(displayPropertyKeysRuleResult)), d2wContext, RuleKeys.componentName))
            case None =>
              val fireDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)
              val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireDisplayPropertyKeys)
              Some(FireRules(KeysSubstrate(ruleFault = Some(ruleFaultListDisplayPropertyKeys)), d2wContext, RuleKeys.componentName))
          }

          val drySubstrateOpt: Option[DrySubstrate] = dataRep match {
            case Some(DataRep(Some(fetchSpecification), _)) =>
              Some(DrySubstrate(fetchSpecification = Some(fetchSpecification)))

            case Some(DataRep(_, Some(eosAtKeyPath))) => {
              Some(DrySubstrate(Some(eosAtKeyPath)))
            }
            case _ => None
          }


          // TODO Hydration should be avoided if the objects are already hydrated
          val hydrationOpt = drySubstrateOpt match {
            case Some(drySubstrate) =>
              displayPropertyKeysRuleResultOpt match {
                case Some(displayPropertyKeysRuleResult) =>
                  val displayPropertyKeys = RuleUtils.ruleListValueWithRuleResult(displayPropertyKeysRuleResultOpt)

                  val isHydrated = Hydration.isHydratedForPropertyKeys(p.proxy.value.eomodel.get,p.proxy.value.cache, drySubstrate, displayPropertyKeys)
                  if (isHydrated) {
                    None
                  } else {
                    Some(Hydration(
                      drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                      WateringScope( // RuleFault
                        ruleResult = Some(displayPropertyKeysRuleResult)
                      )))
                  }
                case None =>
                  log.debug("NVListComponent mounted: drySubstrate: " + drySubstrate)
                  val fireDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)
                  val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireDisplayPropertyKeys)

                  Some(Hydration(
                    drySubstrate, // Hydration of objects at the end of relationship, stored in cache
                    WateringScope( // RuleFault
                      ruleFault = Some(ruleFaultListDisplayPropertyKeys)
                    )))
              }

            case None =>
              None
          }




          val fireActions: List[Option[D2WAction]] = List(
            fireDisplayPropertyKeysOpt, // standard Fire Rule
            fireIsEditAllowedOpt, // standard Fire Rule
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


          log.debug("NVListComponent mounted: FireActions: " + fireActions.size)
          val actions = fireActions.flatten

          Callback.when(!actions.isEmpty)(p.proxy.dispatchCB(
            FireActions(
              d2wContext,
              actions
            )
          ))
        case _ =>
          log.debug("NVListComponent mounted: no entity for entity name: " + entityName)
          Callback.empty
      }

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
      val d2wContext = p.d2wContext

      //log.debug("NVListComponent render |  proxy d2wContext: " + d2wContext)

      val entityName = d2wContext.entityName.get
      log.debug("NVListComponent render for entity: " + entityName)


      val ruleResultsModel = p.proxy.value.ruleResults
      //log.debug("NVListComponent render ruleResultsModel: " + ruleResultsModel)

      val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("NVListComponent render task displayPropertyKeys " + displayPropertyKeys)
      val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

      val isEditAllowedOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isEditAllowed)
      val isEditAllowed = isEditAllowedOpt match {
        case Some(editAllowed) => editAllowed.equals("true")
        case None => false
      }
      val cloneAllowed = false && isEditAllowed // not yet implemented


      val dataRepOpt = d2wContext.dataRep

      //log.debug("dataRepOpt " + dataRepOpt)
      val eos: List[EO] = dataRepOpt match {
        case Some(dataRep) => {
          val cache = p.proxy.value.cache
          dataRep match {
            case DataRep(Some(fs), _) =>
              //log.debug("NVListCompoennt look for objects in cache with fs " + fs)
              //log.debug("NVListCompoennt look for objects in cache " + cache)
              log.debug("NVListCompoennt look for objects in cache with fs")
              EOCacheUtils.objectsWithFetchSpecification(cache, fs)

            case DataRep(_, Some(eosAtKeyPath)) => {
              //log.debug("NVListComponent render eosAtKeyPath " + eosAtKeyPath)
              log.debug("NVListComponent render eosAtKeyPath")
              val eovalueOpt = EOValue.valueForKey(eosAtKeyPath.eo, eosAtKeyPath.keyPath)
              eovalueOpt match {
                case Some(eovalue) =>

                  // ObjectsValue(Vector(1))
                  eovalue match {
                    case ObjectsValue(pks) =>
                      log.debug("NVListComponent render pks " + pks)
                      EOCacheUtils.outOfCacheEOUsingPks(p.proxy.value.cache, entityName, pks).toList
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
      log.debug("NVListComponent render eos " + eos.size)


      val countText = eos.size + " " + (if (entityDisplayNameOpt.isDefined) entityDisplayNameOpt.get else "")

      <.div(^.className := "",
        {
          val eoOnErrorOpt = eos.find(x => x.validationError.isDefined)
          eoOnErrorOpt match {
            case Some(eoOnError) =>
              val validationError = eoOnError.validationError.get
              val objUserDescription = eoOnError.entity.name + " " + eoOnError.values + " : "
              <.div(<.span(^.color := "red", "Validation error with object: " + objUserDescription), <.span(^.color := "red", ^.dangerouslySetInnerHtml := validationError))
            case _ => <.div()
          }
        },
        {
          <.table(^.className := "table table-striped table-bordered table-hover table-condensed",
            <.thead(
              <.tr(^.className := "",
                <.th(^.className := "no-border",
                  <.span(^.className := "attribute", countText)
                ),
                if (p.isEmbedded) <.th(^.className := "no-border") else <.th(^.className := "no-border listHeaderReturnButton", <.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName)))),
                <.th(^.className := "no-border"),
                <.th(^.className := "no-border"),
                <.th(^.className := "no-border")
              )
            ),

            <.thead(
              <.tr(^.className := "",
                <.th(), {
                  displayPropertyKeys toTagMod (propertyKey =>
                    <.th(^.className := "", {
                      val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                      val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
                      val displayString = displayNameFound match {
                        case Some(stringValue) => {
                          //case Some(stringValue) => {
                          stringValue
                        }
                        case _ => propertyKey
                      }
                      <.span(^.className := "", displayString)
                    })
                    )
                },
                <.th()
              )
            ),

            <.tbody(
              eos toTagMod (eo =>
                <.tr(
                  <.td(^.className := "text-center",
                    <.i(^.className := "glyphicon glyphicon-search", ^.title := "inspect", ^.onClick --> inspectEO(eo)),
                    <.i(^.className := "glyphicon glyphicon-pencil", ^.title := "edit", ^.onClick --> editEO(eo)).when(isEditAllowed),
                    <.i(^.className := "glyphicon glyphicon-duplicate", ^.title := "duplicate").when(cloneAllowed)
                  ),
                  displayPropertyKeys toTagMod (
                    propertyKey => {
                      val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                      <.td(^.className := "",
                        //"toto"
                        D2WComponentInstaller(p.router, propertyD2WContext, eo, p.proxy)
                      )
                    }
                    ),
                  //if (p.isEmbedded) <.td() else
                  <.td(^.className := "text-center",
                    <.i(^.className := "glyphicon glyphicon-trash", ^.title := "delete", ^.onClick --> deleteEO(eo))
                  ).when(!p.isEmbedded)
                )
                )
            )


          )
        }
      )

    }
  }


  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, isEmbedded, proxy))


}
