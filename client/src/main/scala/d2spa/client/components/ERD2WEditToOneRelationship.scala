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


sealed trait TodoPriority

case object TodoLow extends TodoPriority

case object TodoNormal extends TodoPriority

case object TodoHigh extends TodoPriority


object ERD2WEditToOneRelationship  {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(p: Props) = {
      println("ERD2WEditToOneRelationship mounted")
      val d2wContext = p.proxy.value.menuModel.get.d2wContext.copy(propertyKey = p.property.d2wContext.propertyKey)
      println("ERD2WEditToOneRelationship mounted: d2wContext" + d2wContext)
      val dataNotFetched = !AppModel.rulesContainsKey(p.property,RuleKeys.keyWhenRelationship)
      println("ERD2WEditToOneRelationship mounted: dataNotFetched" + dataNotFetched)

      val entity = p.property.d2wContext.entity
      val propertyName = p.property.d2wContext.propertyKey
      println("ERD2WEditToOneRelationship mounted: entity" + entity)
      println("ERD2WEditToOneRelationship mounted: entity" + propertyName)

      val eomodelPot = p.proxy.value.eomodel
      eomodelPot match {
        case (Ready(eomodel)) =>
          println("ERD2WEditToOneRelationship mounted: eomodel" + eomodel)
          val destinationEntity = EOModelUtils.destinationEntity(eomodel, entity, propertyName)
          println("ERD2WEditToOneRelationship mounted: destinationEntity" + destinationEntity)
          Callback.when(true)(p.proxy.dispatchCB(HydrateProperty(p.property, List(RuleKeys.keyWhenRelationship)))) >>
            Callback.when(true)(p.proxy.dispatchCB(FetchObjectsForEntity(destinationEntity)))

        case _ =>
          println("ERD2WEditToOneRelationship mounted: eomodel not fetched")
          Callback.when(true)(p.proxy.dispatchCB(HydrateProperty(p.property, List(RuleKeys.keyWhenRelationship))))

      }



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

    def render(p: Props) = {
      val entity = p.property.d2wContext.entity
      val eo = p.eo
      val propertyName = p.property.d2wContext.propertyKey
      //println("Edit To One Relationship " + eo)
      val keyWhenRelationshipRuleOpt = p.property.ruleKeyValues.find(r => {
        r.key.equals(RuleKeys.keyWhenRelationship)
      })
      keyWhenRelationshipRuleOpt match {
        case Some(keyWhenRelationshipRule) => {
          val keyWhenRelationship = keyWhenRelationshipRule.eovalue.stringV.get
          val destinationEntity = EOModelUtils.destinationEntity(p.proxy.value.eomodel.get, entity, propertyName)
          val eoCache = p.proxy.value.eos
          val destinationEOs = if (eoCache.contains(destinationEntity.name)) Some(eoCache(destinationEntity.name)) else None
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
                    p.proxy.dispatchCB(UpdateEOValueForProperty(eo, entity, p.property, EOValue(typeV = ValueType.eoV, eoV = eoRefWith(eos, destinationEntity, e.currentTarget.value))))
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

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, property, eo, proxy))
}
