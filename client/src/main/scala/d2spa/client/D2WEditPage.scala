package d2spa.client

import d2spa.client.components.D2WComponentInstaller
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle

import d2spa.client.SPAMain.{TaskAppPage}

object D2WEditPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      Callback.when(props.proxy().entityMetaDatas.isEmpty)(props.proxy.dispatchCB(InitMenu))
    }


    def save(router: RouterCtl[TaskAppPage],entity: EOEntity,eo: EO) = {
      val hasPk = eo.values.find(value => { value._1.equals(eo.entity.pkAttributeName)}).isDefined
      if (hasPk) {
        Callback.log(s"Save: $entity") >>
          $.props >>= (_.proxy.dispatchCB(Save(entity,eo)))
      } else {
        Callback.log(s"Save: $entity") >>
          $.props >>= (_.proxy.dispatchCB(NewEO(entity,eo)))
      }

    }

    def returnAction (router: RouterCtl[TaskAppPage],entity: EOEntity) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage(entity)))
    }

    /*def eo(propertyKeys: List[EditInspectProperty]): EO = {
       //propertyKeys.filter(p => p.value.value.length > 0).map(p => EOKeyValueQualifier(p.key,p.value.value))
      EO(Map())
    }*/

    def render(p: Props) = {
      val entityName = p.entity
      println("Render Edit page for entity: " + entityName + " and task " + p.task)
      val metaDatas = p.proxy.value
      if  (!metaDatas.entityMetaDatas.isEmpty) {
        val entityMetaData = metaDatas.entityMetaDatas.find(emd => emd.entity.name.equals(entityName)).get
        val entity = entityMetaData.entity
        val isEdit = p.task.equals(TaskDefine.edit)
        val task = if (isEdit) entityMetaData.editTask else entityMetaData.inspectTask
        val displayPropertyKeys = task.displayPropertyKeys
        val banImage = if (isEdit) "/assets/images/EditBan.gif" else "/assets/images/InspectBan.gif"
        val eo = p.proxy.value.eo.getOrElse( {
          val valueMap = entityMetaData.editTask.displayPropertyKeys.map (x => {
            x.d2wContext.propertyKey -> EOValue(typeV = x.typeV)
          }).toMap
          EO(entity,valueMap,Some(1),None)
        })
        println("Edit page EO " + eo)
        <.div(
          <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
          <.div(^.id:="a",
            {
              if (eo.validationError.isDefined) {
                <.div(<.span(^.color:="red",^.dangerouslySetInnerHtml := eo.validationError.get))
              } else <.div()
            },
            <.div(^.className := "banner d2wPage",
              <.span(<.img(^.src := banImage))
            ),
            <.div(^.className :="liner d2wPage",<.img(^.src := "/assets/images/Line.gif")),
            <.div(^.className :="buttonsbar d2wPage",
              <.span(^.className :="buttonsbar attribute beforeFirstButton",entityMetaData.displayName),
              <.span(^.className :="buttonsbar",
                if (isEdit) {
                  <.img(^.src := "/assets/images/ButtonSave.gif",^.onClick --> save(p.router,entity,p.proxy.value.eo.get))
                } else {
                  " "
                },
                if (isEdit) {
                  " "
                } else {
                  <.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router,entity))
                }
              )
            ),
              <.div(^.className :="repetition d2wPage",
                <.table(^.className :="query",
                  <.tbody(
                    <.tr(^.className :="attribute customer",
                      <.td(
                        <.table(
                          <.tbody(
                        displayPropertyKeys toTagMod (property =>
                          <.tr(^.className :="attribute",
                            <.th(^.className :="propertyName query",{
                                val displayNameFound = property.ruleKeyValues.find(r => {r.key.equals(RuleKeys.displayNameForProperty)})
                                val displayString = displayNameFound match {
                                  case Some(ruleResult) => {
                                    ruleResult.eovalue.stringV.get
                                  }
                                  case _ => property.d2wContext.propertyKey
                                }
                                <.span(displayString)
                              }
                            ),
                            <.td(^.className :="query d2wAttributeValueCell",
                              D2WComponentInstaller(p.router,property, eo, p.proxy)
                            ),
                            // DEBUG
                            <.td(^.className :="query d2wAttributeValueCell",
                              {
                                if (p.proxy.value.isDebugMode) {
                                  val componentName = property.ruleKeyValues.filter(r => {r.key.equals(RuleKeys.componentName)}).head.eovalue.stringV.get
                                  componentName
                                } else ""

                              }
                            )
                          )
                        )
                          )
                      )
                    )
                    )
                  )
                )
              )
          )
        )
      } else {
        <.div("no meta datas")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WEditPage")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    //.componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, task, proxy))
  }
}
