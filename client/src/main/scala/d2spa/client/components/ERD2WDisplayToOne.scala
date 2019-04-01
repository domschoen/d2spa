package d2spa.client.components

import d2spa.client.RuleUtils.ruleResultForContextAndKey
import d2spa.client.components.ERD2WEditToOneRelationship.Props
import d2spa.client._
import d2spa.client.logger.{D2SpaLogger, log}
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
//import d2spa.client.css.GlobalStyle

import d2spa.client.MegaContent
import d2spa.client.SPAMain.TaskAppPage
import d2spa.shared.PropertyMetaInfo


object ERD2WDisplayToOne {

  //@inline private def bss = GlobalStyles.bootstrapStyles
  //bss.formControl,
  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])


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

  class Backend($: BackendScope[Props, Unit]) {

    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {

      val cD2WContext = currentProps.d2wContext
      val nD2WContext = nextProps.d2wContext
      val d2wContextChanged = !cD2WContext.equals(nD2WContext)


      val anyChange = d2wContextChanged
      //D2SpaLogger.logfinest( entityName, "ERD2WDisplayToOne willReceiveProps | anyChange: " + anyChange)

      Callback.when(anyChange) {
        mounted(nextProps)
      }
    }

    def mounted(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted " + pageContext)

      val eomodel = p.proxy.value.cache.eomodel.get

      val eoOpt = pageContext.eo
      eoOpt match {
        case Some(eoContaining) =>
          //D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted | eoContaining " + eoContaining)

          val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
          entityOpt match {
            case Some(entity) =>
              val propertyName = d2wContext.propertyKey.get


              val ruleResults = p.proxy.value.ruleResults
              val keyWhenRelationshipRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.keyWhenRelationship)
              val rulesPots = List(keyWhenRelationshipRuleResultPot)
              val rules = RuleUtils.firingRulesFromPotFiredRuleResult(rulesPots)
              val ruleRequestOpt = RuleUtils.ruleRequestWithRules(d2wContext, rules)

              val destinationEOValueOpt = eoContaining.valueForKey(propertyName)
              //D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted | destinationEOValueOpt  " + destinationEOValueOpt)

              destinationEOValueOpt match {

                case Some(destinationEOValue) =>
                  destinationEOValue match {
                    case ObjectValue(destinationEOPk) =>
                      //D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: eomodel: " + eomodel)

                      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                      destinationEntityOpt match {
                        case Some(destinationEntity) =>
                          val destinationEntityName = destinationEntity.name
                          //D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted | destinationEntityName  " + destinationEntityName)
                          val destEOFault = EOFault(destinationEntityName, destinationEOPk)
                          val hydration = Hydration(DrySubstrate(eo = Some(destEOFault)), WateringScope(ruleResult = keyWhenRelationshipRuleResultPot))
                          p.proxy.dispatchCB(HydrationRequest(hydration, ruleRequestOpt))

                        case None =>
                          D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: no destination entity for property: " + propertyName)
                          Callback.empty
                      }
                    case _ =>
                      Callback.empty
                  }
                case None =>
                  D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: no value for property " + propertyName + " in eoContaining: " + eoContaining)
                  Callback.empty
              }
            case None =>
              D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted | no eoContaining")
              D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: no eoContaining " + entityName)
              Callback.empty
          }
        case None =>
          D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | mounted | no eo")
          D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: no eo " + entityName)
          Callback.empty
      }
    }

    def render(p: Props) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne | render " + d2wContext.entityName + " task " + d2wContext.task + " propertyKey " + d2wContext.propertyKey + " page configuration " + d2wContext.pageConfiguration)

      val propertyName = d2wContext.propertyKey.get

      val eoOpt = pageContext.eo
      eoOpt match {
        case Some(eoContaining) =>



          // We expect a value for that property. Either:
          // StringValue
          // EmptyValue
          val destinationEOValueOpt = eoContaining.valueForKey(propertyName)
          destinationEOValueOpt match {
            case Some(destinationEOValue) =>
              destinationEOValue match {
                case ObjectValue(destinationEOPk) =>
                  val eomodel = p.proxy.value.cache.eomodel.get
                  val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
                  entityOpt match {
                    case Some(entity) =>

                      val destinationEntityOpt = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
                      destinationEntityOpt match {
                        case Some(destinationEntity) =>

                          val destinationEntityName = destinationEntity.name
                          //D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne render | get eoContaining out of cache " + destinationEntityName + " eoContaining " + destinationEOPk)
                          val cache = p.proxy.value.cache
                          //log.finest("ERD2WDisplayToOne render | get eoContaining out of cache " + (if (cache.eos.contains(destinationEntityName)) cache.eos(destinationEntityName) else " no cache"))
                          val eoOpt = EOCacheUtils.outOfCacheEOUsingPk(cache, destinationEntityName, destinationEOPk)
                          eoOpt match {
                            case Some(eoContainingFromCache) =>
                              val ruleResultsModel = p.proxy.value.ruleResults
                              val keyWhenRelationshipOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.keyWhenRelationship)
                              keyWhenRelationshipOpt match {
                                case Some(keyWhenRelationship) =>
                                  val eoValueOpt = eoContainingFromCache.valueForKey(keyWhenRelationship)
                                  eoValueOpt match {
                                    case Some(eoValue) =>

                                      val value = EOValue.juiceString(eoValue)
                                      <.div(
                                        <.span(^.id := "description", value))

                                    case None =>
                                      <.div("No value for key " + keyWhenRelationship)
                                  }
                                case None =>
                                  <.div("No keyWhenRelationship")
                              }
                            case None =>
                              <.div("No eoContaining out of cache")
                          }
                        case None =>
                          <.div("No entity for eoContaining")
                      }
                    case None =>
                      <.div("No destination Entity for key: " + propertyName)
                  }

                // This is a nominal case
                case _ =>
                  <.div("")
              }
            case _ =>
              <.div("No value for key " + propertyName)
          }
        case None =>
          D2SpaLogger.logfinest(entityName, "ERD2WDisplayToOne mounted: no eo " + entityName)
          <.div("No eo for entity: " + entityName + " for key " + propertyName)
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("ERD2WDisplayToOne")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps))
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) =
    component(Props(ctl, d2wContext, proxy))

}
