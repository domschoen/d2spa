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


sealed trait TodoPriority

case object TodoLow extends TodoPriority

case object TodoNormal extends TodoPriority

case object TodoHigh extends TodoPriority


object ERD2WEditToOneRelationship  {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class Props(router: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val d2wContext = props.proxy.value.menuModel.get.d2wContext.copy(propertyKey = props.property.d2wContext.propertyKey)
      //val dataNotFetched = !AppModel.rulesContainsKey(props.property,RuleKeys.keyWhenRelationship)
      val dataNotFetched = true
      //Callback.when(dataNotFetched)(props.proxy.dispatchCB(HydrateProperty(props.property,List(RuleKeys.keyWhenRelationship,RuleKeys.destinationEos))))
      Callback.when(dataNotFetched)(props.proxy.dispatchCB(HydrateProperty(props.property, List(RuleKeys.keyWhenRelationship, RuleKeys.destinationEos))))
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
          <.div(
            {
              //println("p.property.ruleKeyValues " + p.property.ruleKeyValues)
              val destinationEntity = EOModelUtils.destinationEntity(p.proxy.value.eomodel.get, entity, propertyName)
              val eoCache = p.proxy.value.eos
              val destinationEOs = if (eoCache.contains(destinationEntity.name)) Some(eoCache(destinationEntity.name)) else None

              destinationEOs match {
                case Some(eos) => {
                  println("eoRefs " + eos)
                  <.div(
                    <.select(bss.formControl, ^.id := "priority", ^.onChange ==> { e: ReactEventFromInput =>
                      p.proxy.dispatchCB(UpdateEOValueForProperty(eo, entity, p.property, EOValue(typeV = ValueType.eoV, eoV = eoRefWith(eos, destinationEntity, e.target.value))))
                    },
                      {
                        val optionTuples = eos map (x => {
                          val id = EOValueUtils.pk(eo)
                          if (id.isDefined) {
                            val displayName = EOValueUtils.stringValueForKey(eo, keyWhenRelationship)
                            Some((id.get,displayName))
                          } else None
                        })
                        // remove None
                        val validTuples = optionTuples.flatten
                        validTuples toTagMod (eo => {
                            <.option(^.value := eo._1, eo._2)
                        })
                      }
                    )
                  )
                }
                case _ => {
                  <.div("")
                }
              }
            }
          )
        }
        case _ => {
          <.div("")
        }
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("ERD2WEditToOneRelationship")
    .renderBackend[Backend]
    //.componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, property, eo, proxy))
}
