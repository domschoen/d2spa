package d2spa.client.components

import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.client._
import d2spa.client.logger.log
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.PropertyMetaInfo


object ERD2WDisplayToOne  {
  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  /*

  EO(
      EOEntity(Project,id,List(EORelationship(customer,Customer))),           // entity
      Map(                                                                    // values
          descr -> EOValue(stringV,Some(1),None,None,Vector()),                   // descr, string, 1
          id -> EOValue(intV,None,Some(1),None,Vector()),                         // id, int, 1
          customer -> EOValue(                                                    // customer, EO(1)
              eoV,
              None,
              None,
              Some(
              EO(
                  EOEntity(Customer,id,List(EORelationship(projects,Project))),
                  Map(
                      id -> EOValue(intV,None,Some(1),None,Vector())
                  ),
                  None,
                  None)
              ),
              Vector()
          ),
          projectNumber -> EOValue(intV,None,Some(1),None,Vector()),               // projectNumber, int, 1
          type -> EOValue(stringV,Some(Project),None,None,Vector())                // type, string, Project (not used)
      ),
      None,                                                                     // memID
      None                                                                      // validationError
  )
  */
// the relationship to customer is a dry object (only the pk)

  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      log.debug("ERD2WEditToOneRelationship mounted")

      val propertyName = p.property.name
      val d2wContext = p.d2wContext.copy(propertyKey = Some(propertyName))
      val dataNotFetched = !RuleUtils.existsRuleResultForContextAndKey(p.property, d2wContext, RuleKeys.keyWhenRelationship)

      log.debug("ERD2WEditToOneRelationship mounted: dataNotFetched" + dataNotFetched)

      val entityName = p.d2wContext.entityName.get
      val eomodel = p.proxy.value.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      log.debug("ERD2WEditToOneRelationship mounted: entity" + entity)
      log.debug("ERD2WEditToOneRelationship mounted: entity" + propertyName)

      log.debug("ERD2WEditToOneRelationship mounted: eomodel" + eomodel)
      val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
      log.debug("ERD2WEditToOneRelationship mounted: destinationEntity" + destinationEntity)


      val keyWhenRelationshipFireRule = FireRule(d2wContext, RuleKeys.keyWhenRelationship)
      val keyWhenRelationshipRuleFault = RuleFault(D2WContextUtils.convertD2WContextToFullFledged(d2wContext), RuleKeys.keyWhenRelationship)
      val destinationEO = EOValueUtils.eoValueForKey(p.eo,propertyName).get
      val destinationPk = EOValueUtils.pk(destinationEO).get
      val destEOFault = EOFault(destinationEO.entity.name,destinationPk)

      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          p.property,
          List[D2WAction](
            keyWhenRelationshipFireRule,
            Hydration(DrySubstrate(eo = Some(destEOFault)),WateringScope(Some(keyWhenRelationshipRuleFault))
          )
        )
      )))
    }

    def render(p: Props) = {
      val eo = p.eo
      val propertyName = p.property.name
      val eoValue = eo.values(propertyName)
      val value = EOValueUtils.juiceString(eoValue)
      <.div(
        <.span(^.id := "description", value)
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("ERD2WDisplayToOne")
    .renderBackend[Backend]
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property,eo, proxy))

}
