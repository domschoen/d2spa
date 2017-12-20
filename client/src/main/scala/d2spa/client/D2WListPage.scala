package d2spa.client


import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import d2spa.client.components.Bootstrap.{Button, CommonStyle}

import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.react.ModelProxy
import d2spa.client.SPAMain.{ListPage, TaskAppPage}
import d2spa.client.components.{D2WComponentInstaller, ERD2WQueryStringOperator, ERD2WQueryToOneField}
import d2spa.shared._
import diode.data.Ready

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], entity: String, task: String, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      println("entity " + props.entity)
      val entityMetaDataNotFetched = props.proxy().entityMetaDatas.indexWhere(n => n.entity.name.equals(props.entity)) < 0
      println("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
      Callback.when(entityMetaDataNotFetched)(props.proxy.dispatchCB(InitMetaData(props.entity)))
    }



    def returnAction (router: RouterCtl[TaskAppPage],entity: EOEntity) = {
      Callback.log(s"Search: $entity") >>
        $.props >>= (_.proxy.dispatchCB(SetupQueryPageForEntity(entity)))
    }
    def inspectEO (eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(InspectEO("list", eo)))
    }

    def editEO (eo: EO) = {
      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(EditEO("list", eo)))
    }

    def deleteEO (eo: EO) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList("list",eo)))
    }


    def render(p: Props) = {
      val entityName = p.entity
      println("Render List page for entity: " + entityName)
      val menuModelPot = p.proxy.value.menuModel
      menuModelPot match {
        case Ready(menuModel) => {
          val entityMetaData = p.proxy.value.entityMetaDatas.find(emd => emd.entity.name.equals(entityName)).get
          val task = entityMetaData.listTask
          val entity = entityMetaData.entity
          //val eos = if (task.eos.isReady) task.eos.get else Vector()
          val eosPot = p.proxy.value.eos
          eosPot match {
            case Ready(eos) =>
              println("list task inside " + eos )
              val displayPropertyKeys = task.displayPropertyKeys
              println("list task displayPropertyKeys " + displayPropertyKeys )
              <.div(
                <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
                <.div(^.id:="a",
                  {
                    val eoOnError = eos.find(x => (x.validationError.isDefined))
                    if (eoOnError.isDefined) {
                      val validationError =  eoOnError.get.validationError.get
                      <.div(<.span(^.color:="red",^.dangerouslySetInnerHtml := validationError))
                    } else <.div()
                  },
                  <.table(^.className := "listPage",
                    <.tbody(
                      <.tr(^.className := "listHeader",
                        <.td(^.className := "listHeaderEntityName",
                          <.span(^.className := "attribute",eos.size + " " + entityMetaData.displayName)
                        ),
                        <.td(^.className := "listHeaderReturnButton",<.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router,entity))))
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
                                    <.img(^.className := "IconButton",^.src := "/assets/images/Write.gif", ^.onClick --> editEO(eo)),
                                    <.img(^.className := "IconButton",^.src := "/assets/images/Clone.gif")
                                  ),
                                  displayPropertyKeys toTagMod (
                                    property =>
                                      <.td(^.className := "list1",
                                        D2WComponentInstaller(p.router,property,eo, p.proxy)
                                      )
                                  ),
                                  <.td(<.img(^.className := "IconButton",^.src := "/assets/images/trashcan-btn.gif", ^.onClick --> deleteEO(eo)))

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
            case _ =>
              <.div(
                <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
                <.div(^.id:="a",""
                )
              )
          }
        }
        case _ => {
          <.div(
            <.div(^.id:="b",MenuHeader(p.router,p.entity,p.proxy)),
            <.div(^.id:="a",""
            )
          )
        }
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("D2WListPage")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
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
