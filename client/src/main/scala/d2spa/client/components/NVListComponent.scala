package d2spa.client.components


import d2spa.client.RuleUtils.firingRulesFromPotFiredRuleResult
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{Button, CommonStyle}
import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.react.ModelProxy
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.client._
import d2spa.shared._
import diode.data.Ready

object NVListComponent {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent])
  case class State(sortOrdering: Option[EOSortOrdering])

  class Backend($: BackendScope[Props, State]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      //log.finest("NVListComponent willReceiveProps | currentProps: " + currentProps)
      //log.finest("NVListComponent willReceiveProps | nextProps: " + nextProps)
      log.finest("NVListComponent willReceiveProps ")

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
      //log.finest("NVListComponent willReceiveProps | anyChange: " + anyChange)

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }


    def willmounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName, "NVListComponent | mounted | entityName: " + entityName)

      val ruleResults = p.proxy.value.ruleResults
      val isEditAllowedRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.isEditAllowed)
      val isInspectAllowedRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.isInspectAllowed)
      val additionalRulesPots = List(isEditAllowedRuleResultPot, isInspectAllowedRuleResultPot)
      val additionalRules = RuleUtils.firingRulesFromPotFiredRuleResult(additionalRulesPots)

      val rules = if (p.isEmbedded) {
        val metaDataRules = RuleUtils.metaDataFiringRules(p.proxy.value.ruleResults, d2wContext)
        metaDataRules ::: additionalRules
      } else {
        additionalRules
      }

      val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)
      D2SpaLogger.logfinest(entityName, "NVListComponent | mounted | ruleRequestOpt: " + ruleRequestOpt)

      if (p.isEmbedded) {
        val ruleResultsModel = p.proxy.value.ruleResults
        D2SpaLogger.logfinest(entityName, "NVListComponent mounted: entity: " + entityName)

        val dataRep = pageContext.dataRep
        val eomodel = p.proxy.value.cache.eomodel.get
        D2SpaLogger.logfinest(entityName, "NVListComponent mounted: dataRep: " + dataRep)

        val drySubstrateOpt: Option[DrySubstrate] = HydrationUtils.drySubstrateFromDataRep(dataRep)
        val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
        val needsHydration = HydrationUtils.needsHydration(drySubstrateOpt, displayPropertyKeysRuleResultPot, p.proxy.value.cache, eomodel)
        D2SpaLogger.logfinest(entityName, "NVListComponent mounted: needsHydration: " + needsHydration)
        if (needsHydration) {
          D2SpaLogger.logfinest(entityName, "NVListComponent mounted: drySubstrateOpt: " + drySubstrateOpt)
          p.proxy.dispatchCB(HydrationRequest(
            Hydration(drySubstrateOpt.get, // Hydration of objects at the end of relationship, not stored in cache
              WateringScope(ruleResult = displayPropertyKeysRuleResultPot)),
            ruleRequestOpt
          ))
        } else {
          Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))
        }
      } else {
        Callback.when(!ruleRequestOpt.isEmpty)(p.proxy.dispatchCB(SendRuleRequest(ruleRequestOpt.get)))
      }
    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eoContaining: EOContaining) = {
      //println("NVListCompoennt InspectEO")
      val eo = eoContaining.eo
      val pageContext = PageContext(d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.inspect)), eo = Some(eoContaining))

      Callback.log(s"Inspect: $eo") >>
        //$.props >>= (_.proxy.dispatchCB(InspectEO(TaskDefine.list, eo, false)))
        $.props >>= (_.proxy.dispatchCB(PrepareEODisplay(pageContext)))
    }

    def editEO(eoContaining: EOContaining) = {
      //val pk = EOValue.pk(eo)
      val eo = eoContaining.eo
      val d2wContext = PageContext(d2wContext = D2WContext(entityName = Some(eo.entityName), task = Some(TaskDefine.edit)), eo = Some(eoContaining))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(PrepareEODisplay(d2wContext)))
    }

    def deleteEO(eoContaining: EOContaining) = {
      Callback.log(s"Delete: $eoContaining") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList(eoContaining)))
    }

    def sortTableColumnProperty(eos: List[EOContaining], propertyKey: String, p: Props, s: State) = {
      val isAscending = if(s.sortOrdering.isDefined) EOSortOrdering.isAscending(s.sortOrdering.get) else false
      val flippedAscending = if (isAscending) EOSortOrdering.CompareDescending else EOSortOrdering.CompareAscending
      println("NVListComponent | sortTableColumnProperty | isAscending " + isAscending + " goes to " + flippedAscending)
      $.modState(_.copy(sortOrdering = Some(EOSortOrdering(propertyKey, flippedAscending))))
    }



    def render(p: Props, s: State) = {

      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext

      //log.finest("NVListComponent render |  proxy d2wContext: " + d2wContext)

      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName, "NVListComponent render for entity: " + entityName)

      val ruleResultsModel = p.proxy.value.ruleResults
      //log.finest("NVListComponent render ruleResultsModel: " + ruleResultsModel)
      D2SpaLogger.logfinest(entityName, "NVListComponent render |  " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)


      val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
      D2SpaLogger.logfinest(entityName, "NVListComponent render task displayPropertyKeys " + displayPropertyKeys)
      val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

      val isInspectAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isInspectAllowed)
      val isEditAllowed = RuleUtils.ruleBooleanValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.isEditAllowed)
      val cloneAllowed = false && isEditAllowed // not yet implemented
      val showFirstCell = isInspectAllowed || isEditAllowed || cloneAllowed

      D2SpaLogger.logfinest(entityName, "NVListComponent render | Inspect: " + isInspectAllowed + " Edit: " + isEditAllowed + " Clone: " + cloneAllowed)

      val dataRepOpt = pageContext.dataRep
      log.finest("dataRepOpt " + dataRepOpt)
      val eos: List[EOContaining] = dataRepOpt match {
        case Some(dataRep) => {
          val cache = p.proxy.value.cache
          dataRep match {
            case DataRep(Some(fs), _) =>
              log.finest("NVListCompoennt look for objects in cache with fs " + fs)
              //log.finest("NVListCompoennt look for objects in cache " + cache)
              D2SpaLogger.logfinest(entityName, "NVListCompoennt look for objects in cache with fs" + cache)
              EOCacheUtils.objectsFromAllCachesWithFetchSpecification(cache, fs)

            case DataRep(_, Some(eosAtKeyPath)) => {
              //log.finest("NVListComponent render eosAtKeyPath " + eosAtKeyPath)
              //D2SpaLogger.logfinest(entityName, "NVListComponent render eosAtKeyPath " + eosAtKeyPath.keyPath)
              //D2SpaLogger.logfinest(entityName, "NVListComponent render eosAtKeyPath | eo " + eosAtKeyPath.eo)
              val eovalueOpt = eosAtKeyPath.eoContaining.valueForKey(eosAtKeyPath.keyPath)
              eovalueOpt match {
                case Some(eovalue) =>

                  // ObjectsValue(Vector(1))
                  eovalue match {
                    case ObjectsValue(pks) =>
                      //D2SpaLogger.logfinest(entityName, "NVListComponent render pks " + pks)
                      EOCacheUtils.outOfCacheEOUsingPks(p.proxy.value.cache, entityName, pks).toList
                    case _ => List.empty[EOContaining]
                  }
                case _ =>
                  D2SpaLogger.logfinest(entityName, "NVListComponent render eosAtKeyPath | eo at key path is None")
                  List.empty[EOContaining]
              }
            }
            case _ => List.empty[EOContaining]
          }
        }
        case _ => List.empty[EOContaining]
      }

      D2SpaLogger.logfinest(entityName, "NVListComponent render eos " + eos.size)

      var dataExist = eos.size > 0
      var csvFileName = "Export"
      val countText = (entityDisplayNameOpt match {
        case Some(entityDisplayName) =>
          csvFileName = entityDisplayName + "s"
          eos.size match {
            case x if x == 0 =>
              "No " + entityDisplayName
            case x if x > 1 =>
              entityDisplayName match {
                case "Alias" =>
                  csvFileName = "Aliases"
                  eos.size + " " + "Aliases"
                case _ => eos.size + " " + entityDisplayName + "s"
              }
            case _ => eos.size + " " + entityDisplayName
          }
        case None => ""
      })

      <.div(^.className := "",
        {
          val eoOnErrorOpt = eos.find(x => x.eo.validationError.isDefined)
          eoOnErrorOpt match {
            case Some(eoOnError) =>
              val validationError = eoOnError.eo.validationError.get
              val objUserDescription = eoOnError.eo.entityName + " " + eoOnError.eo.values + " : "
              <.div(<.span(^.color := "red", "Validation error with object: " + objUserDescription), <.span(^.color := "red", ^.dangerouslySetInnerHtml := validationError))
            case _ => <.div()
          }
        },
        <.div(^.className := "countRow", <.span(^.cursor := "pointer", ^.float := "left", <.span(countText)),
          <.span(^.cursor := "pointer", ^.float := "right", <.a(^.title := csvFileName, ^.className := "export-to-excel glyphicon glyphicon-download-alt")),
          if (p.isEmbedded) "" else <.img(^.className := "text-right", ^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName))),

        <.table(^.className := "nvList table table-bordered table-hover table-condensed",
          <.thead(
            <.tr(^.className := "",
              <.th().when(showFirstCell), {
                displayPropertyKeys toTagMod (propertyKey =>
                  <.th(^.className := "", {
                    val propertyD2WContext = d2wContext.copy(propertyKey = Some(propertyKey))
                    val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
                    val displayString = displayNameFound match {
                      case Some(stringValue) => {
                        stringValue
                      }
                      case _ => propertyKey
                    }
                    val pointerSpan =
                      s.sortOrdering match {
                        case Some(sortOrdering) =>
                          val orderKey = sortOrdering.key
                          if (propertyKey.toString.equals(orderKey)) {

                            val icon: String = if (EOSortOrdering.isAscending(sortOrdering)) {
                              "^.cursor := \"pointer\", ^.float := \"right\", <.span(^.className := icon)-by-attributes"
                            } else {
                              "glyphicon glyphicon-sort-by-attributes-alt"
                            }
                            <.span(^.cursor := "pointer", ^.float := "right", <.span(^.className := icon))

                          } else {
                            <.span(^.cursor := "pointer", ^.float := "right", <.span(^.className := "glyphicon glyphicon-sort"))
                          }

                        case None =>
                          <.span()
                      }

                    <.div(^.onClick --> sortTableColumnProperty(eos, propertyKey.toString, p, s),
                      <.span(^.cursor := "pointer", ^.float := "left", <.span(displayString)),
                      pointerSpan)
                  })
                  )
              },
              <.th().when(!p.isEmbedded)
            )
          ).when(dataExist),

          <.tbody({
            if (dataExist) {
              val cache = p.proxy.value.cache
              //println("NVListComponent | sortEOS | s.sortOrdering " + s.sortOrdering)

              val sortedEos = EOCacheUtils.sortEOS(eos, s.sortOrdering, cache)
              //println("NVListComponent | sortEOS " + sortedEos)

              sortedEos toTagMod (eo => {
                <.tr(
                  <.td(^.className := "text-center",
                    <.i(^.className := "glyphicon glyphicon-search", ^.title := "inspect", ^.onClick --> inspectEO(eo)).when(isInspectAllowed),
                    <.i(^.className := "glyphicon glyphicon-pencil", ^.title := "edit", ^.onClick --> editEO(eo)).when(isEditAllowed),
                    <.i(^.className := "glyphicon glyphicon-duplicate", ^.title := "duplicate").when(cloneAllowed)
                  ).when(showFirstCell),
                  displayPropertyKeys toTagMod (
                    propertyKey => {
                      val propertyD2WContext = d2wContext.copy(propertyKey = Some(propertyKey))
                      val propertyPageContext = pageContext.copy(d2wContext = propertyD2WContext, eo = Some(eo))
                      //println("NVListComponent | propertyPageContext " + propertyPageContext)
                      <.td(^.className := "",
                        D2WComponentInstaller(p.router, propertyPageContext, p.proxy)
                      )
                    }
                    ),
                  <.td(^.className := "text-center",
                    <.i(^.className := "glyphicon glyphicon-trash", ^.title := "delete", ^.onClick --> deleteEO(eo))
                  ).when(!p.isEmbedded)
                )
              }
                )
            } else {
              <.tr(<.td(^.className := "text-center", ^.colSpan := 100, "No records found."))
            }
          }
          )
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .initialState(State(None))
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, isEmbedded, proxy))


}
