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


  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent])

  class Backend($ : BackendScope[Props, Unit]) {

    def mounted(p: Props) = {
      val d2wContext = p.d2wContext

      val entityName = d2wContext.entityName.get

      log.debug("NVListComponent did mount " + d2wContext)

      //val entityName = p.d2wContext.entityName.get
      //log.debug("D2WListPage mounted for entity " + entityName)

      // TODO: Hydration base on displayPropertyKeys

      val entityMetaDataNotFetched = !RuleUtils.metaDataFetched(p.proxy().ruleResults, d2wContext)
      log.debug("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      Callback.when(entityMetaDataNotFetched)(p.proxy.dispatchCB(InitMetaDataForList(entityName)))
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
      log.debug("NVListComponent render ")
      val d2wContext = p.d2wContext

      val entityName = d2wContext.entityName.get
      log.debug("NVListComponent render for entity: " + entityName)


      val ruleResultsModel = p.proxy.value.ruleResults

      val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayPropertyKeys)
      log.debug("NVListComponent render task displayPropertyKeys " + displayPropertyKeys)
      val entityDisplayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, d2wContext, RuleKeys.displayNameForEntity)

      val dataRepOpt = d2wContext.dataRep
      dataRepOpt match {
        case Some(dataRep) =>
          val eosRefs = dataRep match {
            case DataRep(fetchSpecification: EOFetchSpecification,_) =>
              // TODO implementation
              List.empty[EO]

            case DataRep(_,eosAtKeyPath: EOsAtKeyPath) => {
              val eovalueOpt = EOValueUtils.valueForKey(eosAtKeyPath.eo, eosAtKeyPath.keyPath)
              eovalueOpt match {
                case Some(eovalue) =>
                  eovalue.eosV.toList
                case _ =>
                  List.empty[EO]
              }
            }
            case _ => List.empty[EO]
          }
          val eos = EOCacheUtils.outOfCacheEOsUsingPkFromEOs(p.proxy.value, entityName, eosRefs)

          val eosPot: Option[Map[Int, EO]] = if (eos.length == 0) None else {
            // TODO create a map with eos 

            Some(p.proxy.value.cache.eos(entityName))
          }
          eosPot match {
            case Some(eosByID) =>
              log.debug("NVListComponent render eosByID " + eosByID)
              val eos = eosByID.values.toList
              val countText = eos.size + " " + (if (entityDisplayNameOpt.isDefined) entityDisplayNameOpt.get else "")

              <.div(
                {
                  val eoOnError = eos.find(x => (x.validationError.isDefined))
                  if (eoOnError.isDefined) {
                    val validationError = eoOnError.get.validationError.get
                    <.div(<.span(^.color := "red", ^.dangerouslySetInnerHtml := validationError))
                  } else <.div()
                },
                {
                  <.table(^.className := "listPage",
                    <.tbody(
                      <.tr(^.className := "listHeader",
                        <.td(^.className := "listHeaderEntityName",
                          <.span(^.className := "attribute", countText)
                        ),
                        if (p.isEmbedded) <.td() else <.td(^.className := "listHeaderReturnButton", <.span(<.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName))))
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
                                      val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResultsModel, propertyD2WContext, RuleKeys.displayNameForProperty)
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
                                  if (p.isEmbedded) <.td() else <.td(<.img(^.className := "IconButton", ^.src := "/assets/images/trashcan-btn.gif", ^.onClick --> deleteEO(eo)))

                                )
                                )
                            )
                          )
                        )
                      )
                    )
                  )
                }
              )
            case _ => <.div("No eos")

          }
        case _ => <.div("No data rep")
      }


    }
  }

  private val component = ScalaComponent.builder[Props]("NVListComponent")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.mounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, isEmbedded: Boolean, proxy: ModelProxy[MegaContent]) = component(Props(ctl, d2wContext, isEmbedded, proxy))


}
