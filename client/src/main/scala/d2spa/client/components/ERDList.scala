package d2spa.client.components

import d2spa.client.{AppModel, FetchObjectsForEntity, FireRules}
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
      val dataNotFetched = !AppModel.rulesContainsKey(p.property,RuleKeys.keyWhenRelationship)
      log.debug("ERD2WEditToOneRelationship mounted: dataNotFetched" + dataNotFetched)
      val eomodel = p.proxy.value.eomodel.get

      val entityName = p.property.entityName
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val propertyName = p.property.name
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

      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))
      val displayPKeysContext = D2WContext(destinationEntity.name, d2spa.shared.TaskDefine.list , None, None, Some(Left("listConfigurationName")))
      Callback.when(dataNotFetched)(p.proxy.dispatchCB(FireRules(p.property, Map(
          RuleKeys.keyWhenRelationship -> d2wContext,
          RuleKeys.pageConfiguration -> displayPKeysContext
      )))) >>
        Callback.when(true)(p.proxy.dispatchCB(FetchObjectsForEntity(destinationEntity)))
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
