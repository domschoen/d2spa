package d2spa.client.components

import d2spa.client._
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

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])

  // destinationEntityName:
  // contains a switch component (ERD2WSwitchComponent)


  // Possible workflow
  // ERDList ask for full fledge EO at the end of the relationship, with all field needed by displayPropertyKeys
  // ERDList convert EORef into EOs
  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      //val destinationEntity = EOModelUtilsdes
      log.debug("ERD2WEditToOneRelationship mounted")
      val eomodel = p.proxy.value.eomodel.get
      val entityName = p.property.entityName
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val propertyName = p.property.name

      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(p.property, d2wContext, RuleKeys.keyWhenRelationship)
      log.debug("ERD2WEditToOneRelationship mounted: dataNotFetched" + dataNotFetched)

      log.debug("ERD2WEditToOneRelationship mounted: entity" + entity)
      log.debug("ERD2WEditToOneRelationship mounted: entity" + propertyName)

      log.debug("ERD2WEditToOneRelationship mounted: eomodel" + eomodel)
      val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.debug("ERD2WEditToOneRelationship mounted: destinationEntity" + destinationEntity)

      // listConfigurationName
      // Then with D2WContext:
      // - task = 'list'
      // - entity.name = 'Project'
      // - pageConfiguration = <listConfigurationName>

      val fireListConfiguration = FireRule(d2wContext, RuleKeys.listConfigurationName)
      val fireDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)
      val displayPKeysContext = D2WContext(
        Some(destinationEntity.name),
        Some(d2spa.shared.TaskDefine.list) , None, None,
        Some(Left(FireRuleConverter.toRuleFault(fireListConfiguration))))
      val fireListDisplayPropertyKeys = FireRule(displayPKeysContext, RuleKeys.displayPropertyKeys)


      // hydrated destination EOs are simply stored in MegaContent eos
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          p.property,
          List(
            FireRule(d2wContext, RuleKeys.keyWhenRelationship),
            fireDisplayPropertyKeys,
            fireListConfiguration,
            fireListDisplayPropertyKeys,
            // in order to have an EO completed with all attributes for the task,
            // gives the eorefs needed for next action which is EOs for the eorefs according to embedded list display property keys
            Hydration(DrySubstrate(eo = Some(p.eo)),WateringScope(Some(FireRuleConverter.toRuleFault(fireDisplayPropertyKeys)))),
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
            Hydration(DrySubstrate(Some(EORefsDefinition(Some(EOsAtKeyPath(p.eo,p.property.name))))),
              WateringScope(Some(
                FireRuleConverter.toRuleFault(fireListDisplayPropertyKeys))))
          )
          )
        ))
    }

    def render(p: Props) = {
      val eo = p.eo
      val propertyName = p.property.name
      println("ERDList propertyKey: " + propertyName)
      println("ERDList eo.values: " + eo.values)
      val eoValue = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None
      val size = if (eoValue.isDefined) eoValue.get.eosV.size else 0
      //val size = 1
      <.div(size + "Projects " + propertyName)
    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property,eo, proxy))

}
