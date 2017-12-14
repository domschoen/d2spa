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
      Callback.when(dataNotFetched)(props.proxy.dispatchCB(HydrateProperty(props.property,List(RuleKeys.keyWhenRelationship, RuleKeys.destinationEos))))
    }


    def updateValue(e: ReactEventFromInput, p: Props) = {
      // update TodoItem priority
      val newPri = e.currentTarget.value match {
        case p if p == TodoHigh.toString => TodoHigh
        case p if p == TodoNormal.toString => TodoNormal
        case p if p == TodoLow.toString => TodoLow
      }
    }

    def eoRefWith(eoRefs: Seq[EORef], id: String) = {
      //println("id " + id + " class " + id.getClass.getName)
      val idAsInt = id.toInt
      eoRefs.find(eoRef => {eoRef.id.equals(idAsInt)})
    }

    def render(p: Props) = {
      val entity = p.property.d2wContext.entity
      val eo = p.eo
      //println("Edit To One Relationship " + eo)

      <.div({
        //println("p.property.ruleKeyValues " + p.property.ruleKeyValues)

        val result = p.property.ruleKeyValues.find(r => {r.key.equals(RuleKeys.destinationEos)})
        result match {
          case Some(rule) => {
            val eoRefs = rule.eovalue.eosV
            println("eoRefs " + eoRefs)
            <.div(
              <.select(bss.formControl, ^.id := "priority",  ^.onChange ==> { e: ReactEventFromInput =>
                p.proxy.dispatchCB(UpdateEOValueForProperty(eo,entity,p.property, EOValue(typeV = ValueType.eoV, eoV = eoRefWith(eoRefs,e.target.value))))},
                {
                  eoRefs toTagMod (eoRef =>
                    <.option(^.value := eoRef.id, eoRef.displayName)
                    )
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
  }

  private val component = ScalaComponent.builder[Props]("ERD2WEditToOneRelationship")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], property: PropertyMetaInfo, eo: EO, proxy: ModelProxy[MegaContent]) = component(Props(ctl, property, eo, proxy))
}
