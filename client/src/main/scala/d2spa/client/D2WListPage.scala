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
import d2spa.client.logger.log
import d2spa.shared._
import diode.data.Ready

object D2WListPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def mounted(props: Props) = {
      val entityName = props.d2wContext.entityName
      log.debug("D2WListPage mounted for entity " + entityName)
      val entityMetaDataNotFetched = props.proxy().entityMetaDatas.indexWhere(n => n.entity.name.equals(entityName)) < 0
      log.debug("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
      Callback.when(entityMetaDataNotFetched)(props.proxy.dispatchCB(InitMetaDataForList(entityName.get)))
    }



    def returnAction (router: RouterCtl[TaskAppPage],entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }
    def inspectEO (eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(SavedEO("list", eo)))
    }

    def editEO (eo: EO) = {
      val pk = EOValueUtils.pk(eo)
      val d2wContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.edit), eo = Some(D2WContextEO(pk = pk)))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(RegisterPreviousPage(d2wContext)))
    }

    def deleteEO (eo: EO) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList("list",eo)))
    }


    def render(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      log.debug("Render List page for entity: " + entityName)
      val menuModelPot = p.proxy.value.menuModel
      menuModelPot match {
        case Ready(menuModel) => {
          val entityMetaData = p.proxy.value.entityMetaDatas.find(emd => emd.entity.name.equals(entityName)).get
          val task = entityMetaData.listTask
          val entity = entityMetaData.entity
          //val eos = if (task.eos.isReady) task.eos.get else Vector()
              <.div(
                <.div(^.id:="b",MenuHeader(p.router,entityName,p.proxy)),
                <.div(^.id:="a",NVListComponent(p.router,p.d2wContext,p.proxy)

                )
              )
            case _ =>
              <.div(
                <.div(^.id:="b",MenuHeader(p.router, p.d2wContext.entityName.get, p.proxy)),
                <.div(^.id:="a","No matching " + entityName + " records found."
                )
              )
          }
        }
        case _ => {
          <.div(
            <.div(^.id:="b",MenuHeader(p.router, p.d2wContext.entityName.get, p.proxy)),
            <.div(^.id:="a","No matching " + entityName + " records found."
            )
          )
        }
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("D2WListPage")
    .renderBackend[Backend]
    //.componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = {
    log.debug("ctl " + ctl.hashCode())
    component(Props(ctl, d2wContext, proxy))
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
