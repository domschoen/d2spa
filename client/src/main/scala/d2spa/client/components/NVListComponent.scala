package d2spa.client.components

import d2spa.client.D2WListPage.Props
import d2spa.client.{D2WContext, InitMetaDataForList, MegaContent, RuleUtils}
import d2spa.client.SPAMain.TaskAppPage
import d2spa.client.components.ERDList.{Backend, Props}
import d2spa.client.logger.log
import d2spa.shared.{EO, PropertyMetaInfo, RuleKeys}
import diode.react.ModelProxy
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^.{<, ^}

object NVListComponent {


    case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])

  def mounted(props: Props) = {
    val entityName = props.d2wContext.entityName
    log.debug("D2WListPage mounted for entity " + entityName)
    val entityMetaDataNotFetched = props.proxy().entityMetaDatas.indexWhere(n => n.entity.name.equals(entityName)) < 0
    log.debug("entityMetaDataNotFetched " + entityMetaDataNotFetched)
    //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
    Callback.when(entityMetaDataNotFetched)(props.proxy.dispatchCB(InitMetaDataForList(entityName.get)))
  }


  def render(p: Props) = {
    val d2wContext = p.d2wContext

    val entityName = d2wContext.entityName

    val displayPropertyKeysOpt = RuleUtils.ruleStringValueForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
    displayPropertyKeysOpt match {
      case Some()
    }



      val eosPot: Option[Map[Int, EO]] = if (p.proxy.value.cache.eos.contains(entityName)) Some(p.proxy.value.cache.eos(entityName)) else None
    eosPot match {
      case Some(eosByID) =>
        log.debug("list task inside " + eosByID )
        val eos = eosByID.values.toList
        val displayPropertyKeys = task.displayPropertyKeys
        log.debug("list task displayPropertyKeys " + displayPropertyKeys )

        <.div(
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
          <.td(^.className := "listHeaderReturnButton",<.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router,entityName))))
        )
      ),
      <.tbody(
        <.tr(
          <.td(
            <.table(^.className :="listRepetition",
              <.tbody(
                <.tr(^.className :="listRepetitionColumnHeader",
                  <.td(), {
                    displayPropertyKeys toTagMod (property =>
                      <.td(^.className :="listRepetitionColumnHeader", {
                        val d2wContext = p.d2wContext.copy(propertyKey = Some(property.name))
                        val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(property,d2wContext, RuleKeys.keyWhenRelationship)
                        val displayString = displayNameFound match {
                          case Some(stringValue) => {
                            //case Some(stringValue) => {
                            stringValue
                          }
                          case _ => property.name
                        }
                        <.span(^.className :="listRepetitionColumnHeader",displayString)
                      })
                      )
                  }
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
                      property => {
                        val propertyD2wContext = p.d2wContext.copy(propertyKey = Some(property.name))
                        <.td(^.className := "list1",
                          D2WComponentInstaller(p.router, propertyD2wContext, property,eo, p.proxy)
                        )

                      }
                      ),
                    <.td(<.img(^.className := "IconButton",^.src := "/assets/images/trashcan-btn.gif", ^.onClick --> deleteEO(eo)))

                  )
                  )
              )
            )
          )
        )
      )

  }

  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, proxy))


}
