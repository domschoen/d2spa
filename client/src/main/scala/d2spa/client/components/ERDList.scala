package d2spa.client.components

import d2spa.client.{FireRule, _}
import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.client.logger.log
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDList ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDList convert EORef into EOs
  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      //val destinationEntity = EOModelUtilsdes
      log.debug("ERDList mounted")
      val eomodel = p.proxy.value.eomodel.get
      val entityName = p.property.entityName
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val propertyName = p.property.name

      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(p.property, d2wContext, RuleKeys.keyWhenRelationship)
      log.debug("ERDList mounted: dataNotFetched" + dataNotFetched)

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
        Some(d2spa.shared.TaskDefine.list) , None,  None, List(), None,
        Some(Left(FireRuleConverter.toRuleFault(fireListConfiguration))))
      log.debug("ERDList mounted: displayPKeysContext" + displayPKeysContext)
      val fireListDisplayPropertyKeys = FireRule(displayPKeysContext, RuleKeys.displayPropertyKeys)
      log.debug("ERDList mounted: fireListDisplayPropertyKeys" + fireListDisplayPropertyKeys)
      val ruleFaultListDisplayPropertyKeys = FireRuleConverter.toRuleFault(fireListDisplayPropertyKeys)

      // hydrated destination EOs are simply stored in MegaContent eos
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          p.property,
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
                  DrySubstrate(Some(EORefsDefinition(Some(EOsAtKeyPath(p.eo,p.property.name))))), // Hydration of objects at the end of relationship, stored in cache
                  WateringScope(Some(
                    ruleFaultListDisplayPropertyKeys))), // populate with properties to be fired rule
              FireRules(KeysSubstrate(Some(ruleFaultListDisplayPropertyKeys)),displayPKeysContext, RuleKeys.componentName)
          )
          )
        ))
    }

    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value, entityName, p.d2wContext.eo.get)

      eoOpt match {
        case Some(eo) =>
          val propertyName = p.property.name

          val eoValue = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None
          val size = if (eoValue.isDefined) eoValue.get.eosV.size else 0
          //val size = 1
          <.div(size + "Projects " + propertyName)
        case None => <.div("")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
