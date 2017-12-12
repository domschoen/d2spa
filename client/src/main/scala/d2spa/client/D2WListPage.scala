package d2spa.client

import d2spa.client.components.{D2WDisplayNumber, ERD2WDisplayString, ERD2WEditNumber, ERD2WEditString}
import d2spa.shared.{RuleKeys, TaskDefine}
import diode.react.ModelProxy
import diode.Action
import org.scalajs.dom.ext.KeyCode
//import d2spa.client.css.GlobalStyle

import scalacss.ScalaCssReact._
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.components.ERD2WQueryStringOperator
import d2spa.shared.{EO}

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    val componentByName = Map("ERD2WQueryStringOperator" -> ERD2WQueryStringOperator)


    def returnAction (router: RouterCtl[TaskAppPage],entity: String) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(InstallQueryPage(entity)))
    }
    def inspectEO (eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO("list", eo)))
    }

    def render(p: Props) = {
      val entity = p.proxy.value.menuModel.get.d2wContext.entity
      val entityMetaData = p.proxy.value.entityMetaDatas.find(emd => emd.entityName.equals(entity)).get
      val task = entityMetaData.listTask
      //val eos = if (task.eos.isReady) task.eos.get else Vector()
      val eos = p.proxy.value.eos.get
      println("list task inside " + eos )
      val displayPropertyKeys = task.displayPropertyKeys
      println("list task displayPropertyKeys " + displayPropertyKeys )
      <.div(
        <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
        <.div(^.id:="a",
          <.table(^.className := "listPage",
            <.tbody(
              <.tr(^.className := "listHeader",
                <.td(^.className := "listHeaderEntityName",
                  <.span(^.className := "attribute",eos.size + " " + entityMetaData.displayName)
                ),
                <.td(^.className := "listHeaderReturnButton",<.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router,p.entity))))
              )
            ),
            <.tbody(
              <.tr(
                <.td(
                  <.table(^.className :="listRepetition",
                    <.tbody(
                      <.tr(^.className :="listRepetitionColumnHeader",
                        <.td(),
                        displayPropertyKeys toTagMod (property =>
                          <.td(^.className :="listRepetitionColumnHeader",{
                            val displayNameFound = property.ruleKeyValues.find(r => {r.key.equals(RuleKeys.displayNameForProperty)})
                            val displayString = displayNameFound match {
                              case Some(ruleResult) => {
                                ruleResult.eovalue.stringV.get
                              }
                              case _ => property.d2wContext.propertyKey
                            }
                            <.span(^.className :="listRepetitionColumnHeader",displayString)
                          }
                          )
                        )
                      )
                    ),
                    <.tbody(
                      eos toTagMod (eo =>
                        <.tr(
                          <.td(
                            <.img(^.className := "IconButton",^.src := "/assets/images/Magglass.gif", ^.onClick --> inspectEO(eo)),
                            <.img(^.className := "IconButton",^.src := "/assets/images/Write.gif"),
                            <.img(^.className := "IconButton",^.src := "/assets/images/Clone.gif")
                          ),
                          displayPropertyKeys toTagMod (
                          property =>
                            <.td(^.className := "list1",
                              {
                                val componentNameFound = property.ruleKeyValues.find(r => {r.key.equals(RuleKeys.componentName)})
                                componentNameFound match {
                                  case Some(ruleResult) => {
                                    val componentNameOpt = ruleResult.eovalue.stringV

                                    componentNameOpt match {
                                      case Some(componentName) => {
                                        componentName match {
                                          case "ERD2WEditString" => ERD2WEditString(p.router, property, eo, p.proxy)
                                          case "ERD2WEditNumber" => ERD2WEditNumber(p.router, property, eo, p.proxy)
                                          case "D2WDisplayNumber" => D2WDisplayNumber(p.router, property, eo, p.proxy)
                                          case "ERD2WDisplayString" => ERD2WDisplayString(p.router, property, eo, p.proxy)
                                          case _ => "Component not found: " + componentName
                                        }
                                      }
                                      case _ => "Rule Result with empty value for property " + property.d2wContext.propertyKey
                                    }
                                  }
                                  case _ => "No component Rule found for property " + property.d2wContext.propertyKey
                                }
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
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WListPage")
    .renderBackend[Backend]
    //.componentWillMount(scope => scope.props.proxy.dispatchCB(SelectMenu(scope.props.entity)))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, entity, task, proxy))
  }
}


/*
                listTask.eos match {
                   case Ready(eos) => {
                      eos.map(eo =>
                        <.tr(displayPropertyKeys.map(
                          property =>
                            <.td(
                              eo.values(property.key).value
                            )
                          )
                        )
                      )
                   }
                   case Empty => {
                      <.tr(
                        <.td(
                          "It's empty"
                        )
                      )
                   }
                   case Pending(_) => {
                      <.tr(
                        <.td(
                          "It's pending"
                        )
                      )
                   }
                }

 */
