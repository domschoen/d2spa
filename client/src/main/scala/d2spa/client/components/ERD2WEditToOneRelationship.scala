package d2spa.client.components

import d2spa.client._
import d2spa.shared._
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ReactEventFromInput, _}
import d2spa.client.components.Bootstrap._
import d2spa.client.components.GlobalStyles

import scalacss.ScalaCssReact._
import d2spa.client.SPAMain.TaskAppPage
import diode.data.Ready
import d2spa.client.logger._
import d2spa.shared.EOModelUtils

sealed trait TodoPriority

case object TodoLow extends TodoPriority

case object TodoNormal extends TodoPriority

case object TodoHigh extends TodoPriority


object ERD2WEditToOneRelationship   {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit])  {
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


      val keyWhenRelationshipRuleFault = RuleFault(RuleUtils.convertD2WContextToFullFledged(d2wContext), RuleKeys.keyWhenRelationship)
      val fireDisplayPropertyKeys = RuleFault(RuleUtils.convertD2WContextToFullFledged(d2wContext), RuleKeys.displayPropertyKeys)
      val keyWhenRelationshipFireRule = FireRule(d2wContext, RuleKeys.keyWhenRelationship)

      Callback.when(dataNotFetched)(p.proxy.dispatchCB(
        FireActions(
          p.property,
          List[D2WAction](
            keyWhenRelationshipFireRule,
            Hydration(DrySubstrate(eo = Some(p.eo)),WateringScope(Some(fireDisplayPropertyKeys))),
            Hydration(DrySubstrate(fetchSpecification = Some(FetchSpecification(destinationEntity.name, None))),WateringScope(Some(keyWhenRelationshipRuleFault)))
          )
        )
      ))
    }


    def updateValue(e: ReactEventFromInput, p: Props) = {
      // update TodoItem priority
      val newPri = e.currentTarget.value match {
        case p if p == TodoHigh.toString => TodoHigh
        case p if p == TodoNormal.toString => TodoNormal
        case p if p == TodoLow.toString => TodoLow
      }
    }

    def eoRefWith(eos: Seq[EO], entity: EOEntity, id: String) = {
      //println("id " + id + " class " + id.getClass.getName)
      if (id.equals("None")) None
      val idAsInt = id.toInt
      val pkAttributeName = entity.pkAttributeName
      val optEO = eos.find(eo => {
        val optPk = EOValueUtils.pk(eo)
        optPk.isDefined && optPk.get.equals(idAsInt)
      })
      if (optEO.isDefined) Some(EORef(entity.name, EOValueUtils.pk(optEO.get).get)) else None
    }
    def d2wContext(props: Props) = props.d2wContext.copy(propertyKey = Some(props.property.name))

    def render(p: Props) = {
      log.debug("ERD2WEditToOneRelationship render")
      val entityName = p.d2wContext.entityName.get
      val eomodel = p.proxy.value.eomodel.get
      val entity = EOModelUtils.entityNamed(eomodel,entityName).get
      val eo = p.eo
      val propertyName = p.property.name
      //val properyD2WContext = RuleUtils.convertD2WContextToFullFledged(d2wContext(p))
      val properyD2WContext = d2wContext(p)


      log.debug("+ rules " + p.property.ruleResults)

      //println("Edit To One Relationship " + eo)
      val keyWhenRelationshipRuleOpt = RuleUtils.ruleStringValueForContextAndKey(p.property,properyD2WContext, RuleKeys.keyWhenRelationship)

      keyWhenRelationshipRuleOpt match {
        case Some(Some(keyWhenRelationship)) => {
        //case Some(keyWhenRelationship) => {
          val destinationEntity = EOModelUtils.destinationEntity(p.proxy.value.eomodel.get, entity, propertyName)
          val eoCache = p.proxy.value.eos
          val destinationEOs = EOCacheUtils.objectsForEntityNamed(eoCache,destinationEntity.name)
          <.div(
            //{
              //println("p.property.ruleKeyValues " + p.property.ruleKeyValues)
           /*   <.div("destinationEntity " + p.proxy.value.eomodel.get +  " destinationEOs "),
            <.div("entity " +entity),
            <.div("propertyName " +propertyName)*/

            destinationEOs match {
              case Some(eos) => {
                println("eoRefs " + eos)
                val destinationEO = EOValueUtils.valueForKey(eo,propertyName)
                val defaultValue = destinationEO match {
                  case Some(EOValue(_,_,_,eoV,_)) => eoV match {
                    case Some(EORef(_,id)) => id.toString()
                    case _ => "None"
                  }
                  case _ => "None"
                }
                <.div(
                  <.select(bss.formControl, ^.value := defaultValue, ^.id := "priority", ^.onChange ==> { e: ReactEventFromInput =>
                    p.proxy.dispatchCB(UpdateEOValueForProperty(eo, entityName, p.property, EOValue(typeV = ValueType.eoV, eoV = eoRefWith(eos, destinationEntity, e.currentTarget.value))))
                  },
                  {
                      val tupleOpts = eos map (x => {

                        val id = EOValueUtils.pk(x)
                        println("id " + id + " for eo: " + x)
                        if (id.isDefined) {
                          val displayName = EOValueUtils.stringValueForKey(x, keyWhenRelationship)
                          Some((id.get.toString,displayName))
                        } else None
                      })
                      // remove None
                      val tupleValids = tupleOpts.flatten.toList
                      val tuplesWithNone = ("None", "- none -") :: tupleValids
                      println("valid tuples " + tupleValids)
                      tuplesWithNone toTagMod (eo => {
                          <.option(^.value := eo._1, eo._2)
                      })
                    }
                  )
                )
              }
              case _ => {
                <.div("No eos for destination entity " + destinationEntity)
              }
            }

          )
        }
        case _ => {
          <.div("keyWhenRelationshipRule is None")
        }
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("ERD2WEditToOneRelationship")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, property, eo, proxy))
}
