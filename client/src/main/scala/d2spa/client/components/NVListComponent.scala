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

    def mounted(p: Props) = {
      //val destinationEntity = EOModelUtilsdes
      log.debug("ERDList mounted")
      val eomodel = p.proxy.value.eomodel.get
      val d2wContext = p.d2wContext
      val entityName = d2wContext.entityName.get
      val propertyName = d2wContext.propertyKey.get
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val ruleResultsModel = p.proxy.value.ruleResults

      //val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
      //log.debug("ERDList mounted: dataNotFetched" + dataNotFetched)

      log.debug("ERDList mounted: entity" + entity)
      log.debug("ERDList mounted: entity" + propertyName)

      log.debug("ERDList mounted: eomodel" + eomodel)
      val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.debug("ERDList mounted: destinationEntity" + destinationEntity)

      // listConfigurationName
      // Then with D2WContext:
      // - task = 'list'
      // - entity.name = 'Project'
      // - pageConfiguration = <listConfigurationName>
      log.debug("ERDList mounted: d2wContext" + d2wContext)

      val fireListConfiguration = FireRule(d2wContext, RuleKeys.listConfigurationName)
      log.debug("ERDList mounted: fireListConfiguration" + fireListConfiguration)
      val fireDisplayPropertyKeys = FireRule(p.d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("ERDList mounted: fireDisplayPropertyKeys" + fireDisplayPropertyKeys)

      // TBD should we use D2WContext or full fledged ?
      val displayPKeysContext = D2WContext(
        Some(destinationEntity.name),
        Some(d2spa.shared.TaskDefine.list) , None,  None, List(), None, None,
        Some(Left(FireRuleConverter.toRuleFault(fireListConfiguration))))
      log.debug("ERDList mounted: displayPKeysContext" + displayPKeysContext)
      val fireListDisplayPropertyKeys = FireRule(displayPKeysContext, RuleKeys.displayPropertyKeys)
      log.debug("ERDList mounted: fireListDisplayPropertyKeys" + fireListDisplayPropertyKeys)
      val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireListDisplayPropertyKeys)


      // case class DataRep (fetchSpecification: Option[EOFetchSpecification] = None, eosAtKeyPath: Option[EOsAtKeyPath] = None)

      val dataRep = d2wContext.dataRep
      val drySubstrate: Option [DrySubstrate] = dataRep match {
        case Some(DataRep(Some(fetchSpecification),_)) =>
          Some(DrySubstrate(fetchSpecification = Some(fetchSpecification)))
        case Some(DataRep(_,Some(eosAtKeyPath))) =>
          d2wContext.eo match {
            case Some(aneo) => Some(DrySubstrate(Some(EOsAtKeyPath(aneo,propertyName))))
            case _ => None
          }
        case _ => None
      }

      Callback.when(drySubstrate.isDefined)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          List(
            fireListConfiguration, // standard FieRule
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
              drySubstrate.get, // Hydration of objects at the end of relationship, stored in cache
              WateringScope(Some(
                ruleFaultListDisplayPropertyKeys))), // populate with properties to be fired rule
            FireRules(KeysSubstrate(Some(ruleFaultListDisplayPropertyKeys)),displayPKeysContext, RuleKeys.componentName)
          )
        )
      ))

    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(SavedEO("list", eo)))
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

      val entityName = d2wContext.entityName.get
      log.debug("NVListComponent render for entity: " + entityName)


      val ruleResultsModel = p.proxy.value.ruleResults

      val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("NVListComponent render task displayPropertyKeys " + displayPropertyKeys)
      val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

      val dataRepOpt = d2wContext.dataRep
      val eos: List[EO] = dataRepOpt match {
        case Some(dataRep) => {
          val cache = p.proxy.value.cache
          dataRep match {
            case DataRep(fetchSpecification: EOFetchSpecification, _) =>
              EOCacheUtils.objectsWithFetchSpecification(cache, fetchSpecification)
            case DataRep(_, eosAtKeyPath: EOsAtKeyPath) => {
              val eovalueOpt = EOValue.valueForKey(eosAtKeyPath.eo, eosAtKeyPath.keyPath)
              eovalueOpt match {
                case Some(eovalue) =>
                  eovalue match {
                    // To Restore case ObjectsValue(eos) => eos.toList
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
                              <.td(^.className := "list1","ll"
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
    }
  }


  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, isEmbedded, proxy))


}
