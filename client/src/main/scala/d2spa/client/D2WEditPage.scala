package d2spa.client

import d2spa.client.components.D2WComponentInstaller
import d2spa.shared._
import diode.react.ModelProxy
import diode.Action
import diode.data.Ready
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import scalacss.ScalaCssReact._
//import d2spa.client.css.GlobalStyle
import scala.collection.immutable.Set
import d2spa.client.logger._

import d2spa.client.SPAMain.{TaskAppPage}

object D2WEditPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {


    // If we go from D2WEditPage to D2WEdtiPage, it will not trigger the willMount
    // To cope with this problem, we check if there is any change to the props and then call the willMount
    def willReceiveProps(currentProps: Props, nextProps: Props): Callback = {
      log.debug("D2WEditPage | willReceiveProps | currentProps: " + currentProps)
      log.debug("D2WEditPage | willReceiveProps | nextProps: " + nextProps)

      val cTask = currentProps.d2wContext.task
      val nTask = nextProps.d2wContext.task
      val taskChanged = !cTask.equals(nTask)

      // may not be up to date ? (we should take the eo from the proxy)
      val cPk = currentProps.d2wContext.eo
      val nPk = nextProps.d2wContext.eo
      val pkChanged = !nPk.equals(nPk)

      val anyChange = taskChanged || pkChanged

      Callback.when(anyChange) {
        willmounted(nextProps)
      }
    }


    // Page do a WillMount and components do a DidMount in order to have the page first (eo hydration has to be done first)
    def willmounted(p: Props) = {

      val d2wContext = p.d2wContext

      val entityName = d2wContext.entityName.get
      log.debug("D2WEditPage: will Mount " + entityName)

      log.debug("D2WEditPage: willMount eo " + d2wContext.eo)

      val entityMetaDataNotFetched = !RuleUtils.metaDataFetched(p.proxy().ruleResults, d2wContext)

      log.debug("D2WEditPage: willMount entityMetaDataNotFetched " + entityMetaDataNotFetched)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
      val fireDisplayPropertyKeys = FireRule(d2wContext, RuleKeys.displayPropertyKeys)


      lazy val noneFireActions = List(
        fireDisplayPropertyKeys,
        // in order to have an EO completed with all attributes for the task,
        // gives the eorefs needed for next action which is EOs for the eorefs according to embedded list display property keys
        CreateMemID(entityName)
      )


      val eoOpt = d2wContext.eo
      val actionList = eoOpt match {
        case Some(eo) =>
          if (EOValue.isNew(eo.pk)) {
            List(
              fireDisplayPropertyKeys
            )
          } else {
            val eoFault = EOFault(entityName, eo.pk)
            List(
              fireDisplayPropertyKeys,
              // in order to have an EO completed with all attributes for the task,
              // gives the eorefs needed for next action which is EOs for the eorefs according to embedded list display property keys
              Hydration(DrySubstrate(eo = Some(eoFault)), WateringScope(ruleResult = PotFiredRuleResult(Left(fireDisplayPropertyKeys))))
            )
          }
        case None => noneFireActions
      }
      val actionList2 = if (entityMetaDataNotFetched) FetchMetaData(d2wContext) :: actionList else actionList
      log.debug("D2WEditPage: willMount actionList " + actionList2)
      val callIt = !actionList2.isEmpty

      Callback.when(callIt)(p.proxy.dispatchCB(
        FireActions(
          d2wContext,
          actionList2
        )
      ))

    }


    def save(router: RouterCtl[TaskAppPage], entityName: String, eo: EO) = {

      val isNewEO = EOValue.isNew(eo)
      if (isNewEO) {
        Callback.log(s"Save new EO: $entityName") >>
          $.props >>= (_.proxy.dispatchCB(SaveNewEO(entityName, eo)))
      } else {
        Callback.log(s"Save: $entityName") >>
          $.props >>= (_.proxy.dispatchCB(Save(entityName, eo)))
      }

    }

    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    /*def eo(propertyKeys: List[EditInspectProperty]): EO = {
       //propertyKeys.filter(p => p.value.value.length > 0).map(p => EOKeyValueQualifier(p.key,p.value.value))
      EO(Map())
    }*/

    def isEdit(p: Props) = p.d2wContext.task.get.equals(TaskDefine.edit)


    def displayPropertyKeysFromProps(p: Props, d2wContext: D2WContext) = {
      RuleUtils.ruleListValueForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
    }


    def render(p: Props) = {

      val staleD2WContext = p.d2wContext
      val entityName = staleD2WContext.entityName.get
      log.debug("D2WEditPage: render eo for entity Name: " + staleD2WContext)


      val d2wContext = p.d2wContext
      //log.debug("D2WEditPage: render eo with fresh context : " + d2wContext)

      val eoRefOpt = d2wContext.eo
      eoRefOpt match {
        case Some(eoRef) =>
          //log.debug("D2WEditPage: render eo | inserted eos " + p.proxy.value.cache.insertedEOs)
          //log.debug("D2WEditPage: render eo | db eos " + p.proxy.value.cache.eos)

          val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromD2WContextEO(p.proxy.value.cache, d2wContext.entityName.get, eoRef)
          log.debug("D2WEditPage: render eo out of cache: " + eoOpt)

          eoOpt match {
            case Some(eo) =>
              val entityName = p.d2wContext.entityName.get
              val ruleResults = p.proxy.value.ruleResults

              log.debug("D2WEditPage: render check meta data fetched with d2wContext " + d2wContext)
              log.debug("D2WEditPage: render check meta data fetched in rules " + ruleResults)
              val metaDataPresent = RuleUtils.metaDataFetched(ruleResults, d2wContext)

              if (metaDataPresent) {
                log.debug("entityMetaDatas not empty")

                //log.debug("Entity meta Data " + metaDatas)
                val displayPropertyKeys = displayPropertyKeysFromProps(p, d2wContext)
                val banImage = if (isEdit(p)) "/assets/images/EditBan.gif" else "/assets/images/InspectBan.gif"
                val displayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, RuleKeys.displayNameForEntity)
                val displayName = if (displayNameOpt.isDefined) displayNameOpt.get else ""

                log.debug("Edit page EO " + eo)
                <.div(
                  <.div(^.id := "b", MenuHeader(p.router, p.d2wContext.entityName.get, p.proxy)),
                  <.div(^.id := "a",
                    {
                      if (eo.validationError.isDefined) {
                        <.div(<.span(^.color := "red", ^.dangerouslySetInnerHtml := eo.validationError.get))
                      } else <.div()
                    },
                    <.div(^.className := "banner d2wPage",
                      <.span(<.img(^.src := banImage))
                    ),
                    <.div(^.className := "liner d2wPage", <.img(^.src := "/assets/images/Line.gif")),
                    <.div(^.className := "buttonsbar d2wPage",
                      <.span(^.className := "buttonsbar attribute beforeFirstButton", displayName),
                      <.span(^.className := "buttonsbar",
                        if (isEdit(p)) {
                          <.img(^.src := "/assets/images/ButtonSave.gif", ^.onClick --> save(p.router, entityName, eo))
                        } else {
                          " "
                        },
                        if (isEdit(p)) {
                          " "
                        } else {
                          <.img(^.src := "/assets/images/ButtonReturn.gif", ^.onClick --> returnAction(p.router, entityName))
                        }
                      )
                    ),
                    <.div(^.className := "repetition d2wPage",
                      <.table(^.className := "query",
                        <.tbody(
                          <.tr(^.className := "attribute customer",
                            <.td(
                              <.table(
                                <.tbody(
                                  displayPropertyKeys toTagMod (property => {
                                    val propertyD2WContext = d2wContext.copy(propertyKey = Some(property))
                                    <.tr(^.className := "attribute",
                                      <.th(^.className := "propertyName query", {
                                        val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(ruleResults, propertyD2WContext, RuleKeys.displayNameForProperty)
                                        val displayString = displayNameFound match {
                                          case Some(stringValule) => {
                                            //case Some(stringValule) => {
                                            stringValule
                                          }
                                          case _ => property
                                        }
                                        <.span(displayString)
                                      }),
                                      <.td(^.className := "query d2wAttributeValueCell",
                                        D2WComponentInstaller(p.router, propertyD2WContext, p.proxy)
                                      )
                                    )
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
              } else {
                <.div("no meta datas " + d2wContext)
              }
            case None => <.div("Object not found in cache")
          }
        case _ => <.div("Object Ref not found")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WEditPage")
    .renderBackend[Backend]
    .componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps,scope.nextProps))
    .componentWillMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, proxy: ModelProxy[MegaContent]) = {
    component(Props(ctl, d2wContext, proxy))
  }
}
