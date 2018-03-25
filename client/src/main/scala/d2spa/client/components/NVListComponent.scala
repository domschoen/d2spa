package d2spa.client.components

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
import d2spa.client._
import d2spa.shared._
import diode.data.Ready

object NVListComponent {


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  class Backend($ : BackendScope[Props, Unit]) {

    def mounted(p: Props) = {
      val entityName = p.d2wContext.entityName
      log.debug("D2WListPage mounted for entity " + entityName)
      val metaDataPresent = RuleUtils.metaDataFetched(p.proxy().ruleResults,p.d2wContext)
      log.debug("entityMetaDataNotFetched " + metaDataPresent)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
      Callback.when(metaDataPresent)(p.proxy.dispatchCB(InitMetaDataForList(entityName.get)))
    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def inspectEO(eo: EO) = {
      Callback.log(s"Inspect: $eo") >>
        $.props >>= (_.proxy.dispatchCB(SavedEO("list", eo)))
    }

    def editEO(eo: EO) = {
      val pk = EOValueUtils.pk(eo)
      val d2wContext = D2WContext(entityName = Some(eo.entity.name), task = Some(TaskDefine.edit), eo = Some(D2WContextEO(pk = pk)))

      Callback.log(s"Edit: $eo") >>
        $.props >>= (_.proxy.dispatchCB(RegisterPreviousPage(d2wContext)))
    }

    def deleteEO(eo: EO) = {
      Callback.log(s"Delete: $eo") >>
        $.props >>= (_.proxy.dispatchCB(DeleteEOFromList("list", eo)))
    }

    def render(p: Props) = {
      val d2wContext = p.d2wContext

      val entityName = d2wContext.entityName.get
      val ruleResultsModel = p.proxy.value.ruleResults

      val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("list task displayPropertyKeys " + displayPropertyKeys)
      val entityDisplayName = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)
      val eosPot: Option[Map[Int, EO]] = if (p.proxy.value.cache.eos.contains(entityName)) Some(p.proxy.value.cache.eos(entityName)) else None
      eosPot match {
        case Some(eosByID) =>
          log.debug("list task inside " + eosByID)
          val eos = eosByID.values.toList
          val countText = eos.size + " " + entityDisplayName

          <.div(
            {
              val eoOnError = eos.find(x => (x.validationError.isDefined))
              if (eoOnError.isDefined) {
                val validationError = eoOnError.get.validationError.get
                <.div(<.span(^.color := "red", ^.dangerouslySetInnerHtml := validationError))
              } else <.div()
            },
            <.table(^.className := "listPage",
              <.tbody(
                <.tr(^.className := "listHeader",
                  <.td(^.className := "listHeaderEntityName",
                    <.span(^.className := "attribute", countText)
                  ),
                  <.td(^.className := "listHeaderReturnButton", <.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName))))
                )
              ),
              <.tbody(
                <.tr(
                  <.td(
                    <.table(^.className := "listRepetition",
                      <.tbody(
                        <.tr(^.className := "listRepetitionColumnHeader",
                          <.td(), {
                            displayPropertyKeys toTagMod (propertyKey =>
                              <.td(^.className := "listRepetitionColumnHeader", {
                                val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                                val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.keyWhenRelationship)
                                val displayString = displayNameFound match {
                                  case Some(stringValue) => {
                                    //case Some(stringValue) => {
                                    stringValue
                                  }
                                  case _ => propertyKey
                                }
                                <.span(^.className := "listRepetitionColumnHeader", displayString)
                              })
                              )
                          }
                        )
                      ),
                      <.tbody(
                        eos toTagMod (eo =>
                          <.tr(
                            <.td(
                              <.img(^.className := "IconButton", ^.src := "/assets/images/Magglass.gif", ^.onClick --> inspectEO(eo)),
                              <.img(^.className := "IconButton", ^.src := "/assets/images/Write.gif", ^.onClick --> editEO(eo)),
                              <.img(^.className := "IconButton", ^.src := "/assets/images/Clone.gif")
                            ),
                            displayPropertyKeys toTagMod (
                              propertyKey => {
                                val propertyD2WContext = p.d2wContext.copy(propertyKey = Some(propertyKey))
                                <.td(^.className := "list1",
                                  D2WComponentInstaller(p.router, propertyD2WContext, eo, p.proxy)
                                )

                              }
                              ),
                            <.td(<.img(^.className := "IconButton", ^.src := "/assets/images/trashcan-btn.gif", ^.onClick --> deleteEO(eo)))

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
  }

  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
