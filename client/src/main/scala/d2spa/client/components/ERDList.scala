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
  class Backend($: BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      val d2wContext = p.d2wContext
      val fireListConfiguration = FireRule(d2wContext, RuleKeys.listConfigurationName)
      log.debug("ERDList mounted: fireListConfiguration: " + fireListConfiguration)

      val fireActions =
        List(
          fireListConfiguration // standard FieRule
        )
      Callback.when(!fireActions.isEmpty)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          fireActions
        )
      ))

    }

    def render(p: Props) = {
      val staleD2WContext = p.d2wContext
      val entityName = staleD2WContext.entityName.get

      val d2wContextOpt = p.proxy.value.previousPage
      log.debug("ERDList render d2wContextOpt " + d2wContextOpt)

      d2wContextOpt match {
        case Some(d2wContext) =>



          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value, entityName, d2wContext.eo.get)

          eoOpt match {
            case Some(eo) =>
              val propertyName = staleD2WContext.propertyKey.get
              val eomodel = p.proxy.value.eomodel.get
              val entity = EOModelUtils.entityNamed(eomodel, entityName).get
              val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
              val destinationEntityName = destinationEntity.name
              val ruleResultsModel = p.proxy.value.ruleResults
              val listConfigurationNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.listConfigurationName)

              //val eoValueOpt = if (eo.values.contains(propertyName)) Some(eo.values(propertyName)) else None

              //val size = eoValueOpt match {
              //  case Some(ObjectsValue(eos)) => eos.size
              //  case _ => 0
              //}
              //val size = 1


              // D2WContext with
              // - Entity (destinationEntity)
              // - task = list
              // - DataRep
              // (the rest is None: previousTask, eo, queryValues, propertyKey, pageConfiguration)
              val embeddedListD2WContext = D2WContext(
                entityName = Some(destinationEntityName),
                task = Some(TaskDefine.list),
                dataRep = Some(DataRep(eosAtKeyPath = Some(EOsAtKeyPath(eo, propertyName)))),
                pageConfiguration = if (listConfigurationNameOpt.isDefined) Some(Right(listConfigurationNameOpt.get)) else None
              )
              log.debug("ERDList render embedded list with context " + embeddedListD2WContext)
              <.div(NVListComponent(p.router, embeddedListD2WContext, true, p.proxy))

            //<.div(size + " " + destinationEntityName + " " + propertyName)
            case None => <.div("")
          }
        case None => <.div("no context")
      }

    }
  }

  private val component = ScalaComponent.builder[Props]("ERDList")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, eo, proxy))

}
