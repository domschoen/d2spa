package d2spa.client

import d2spa.client.EOCacheUtils
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




  case class Props(router: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent])
  case class State(initialized: Boolean)

  val allowedTasks = Set(TaskDefine.edit, TaskDefine.inspect)

  protected class Backend($: BackendScope[Props, State]) {

    def willReceiveProps(currentProps: Props, nextProps: Props, s:State): Callback = {
      log.finest("D2WEditPage | willReceiveProps | currentProps: " + currentProps)
      log.finest("D2WEditPage | willReceiveProps | nextProps: " + nextProps)
      val cTask = currentProps.d2wContext.d2wContext.task
      val nTask = nextProps.d2wContext.d2wContext.task
      val taskChanged = !cTask.equals(nTask)

      // may not be up to date ? (we should take the eo from the proxy)
      val cPk = currentProps.d2wContext.d2wContext.eo
      val nPk = nextProps.d2wContext.d2wContext.eo
      val pkChanged = !nPk.equals(nPk)

      val cPP = currentProps.proxy.value.previousPage
      val nPP = nextProps.proxy.value.previousPage
      val previousPageChanged = !cPP.equals(nPP)



      val anyChange = taskChanged || pkChanged || previousPageChanged

      //Callback.when(anyChange) {
        willmounted(nextProps, s)
      //}
      //willmounted(nextProps)
    }

    // Page do a WillMount and components do a DidMount in order to have the page first (eo hydration has to be done first)
    def willmounted(p: Props, s: State) = {
      val pageContext = p.d2wContext
      val d2wContext = pageContext.d2wContext
      val entityName = d2wContext.entityName.get
      log.finest("D2WEditPage | mounted " + entityName + "state " + s)
      log.finest("D2WEditPage | mounted eo " + d2wContext.eo)
      log.finest("D2WEditPage | mounted d2wContext " + d2wContext)
      log.finest("D2WEditPage | mounted proxy d2wContext " + p.proxy.value.previousPage)
      val task = d2wContext.task.get
      if (!allowedTasks.contains(task)) {
        log.finest("D2WEditPage | mounted | task not allowed " + task)
        Callback.empty
      } else {
        val socketReady = p.proxy.value.appConfiguration.socketReady

        if (!socketReady) {
          log.finest("D2WEditPage | mounted | socket not ready")
          Callback.empty
        } else {
          p.proxy.dispatchCB(PrepareEODisplay(pageContext))
        }
      }
    }

    def clear =  {
      println("CCCOOMMMPPPOOONNEEENNT UNNNNNMMMMMOOOUUUNNNTTTT")
      Callback.empty
    }


    def save(router: RouterCtl[TaskAppPage], entityName: String, eo: EO) = {

      val isNewEO = EOValue.isNewEO(eo)
      if (isNewEO) {
        Callback.log(s"Save new EO: $entityName") >>
          $.props >>= (_.proxy.dispatchCB(SaveNewEO(entityName, eo)))
      } else {
        Callback.log(s"Save: $entityName") >>
          $.props >>= (_.proxy.dispatchCB(Save(entityName, eo)))
      }

    }
    def isEdit(p: Props) = p.d2wContext.d2wContext.task.get.equals(TaskDefine.edit)


    def displayPropertyKeysFromProps(p: Props, d2wContext: D2WContext) = {
      RuleUtils.ruleListValueForContextAndKey(p.proxy.value.ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
    }



    def returnAction(router: RouterCtl[TaskAppPage], entityName: String) = {
      Callback.log(s"Search: $entityName") >>
        $.props >>= (_.proxy.dispatchCB(SetPreviousPage))
    }

    def render(p: Props, s: State) = {

      val staleD2WContext = p.d2wContext.d2wContext
      val entityName = staleD2WContext.entityName.get
      log.finest("D2WEditPage: render eo for entity Name: " + staleD2WContext)

      log.finest("D2WEditPage: render | eocache : " + p.proxy.value.cache)
      log.finest("D2WEditPage: render | p.proxy.value.previousPage : " + p.proxy.value.previousPage)

      p.proxy.value.previousPage match {
        case Some(pageContext) =>
          val d2wContext = pageContext.d2wContext
          log.finest("D2WEditPage: render eo with fresh context : " + d2wContext)

          val eoRefOpt = d2wContext.eo
          log.finest("D2WEditPage: render eo : " + eoRefOpt)

          eoRefOpt match {
            case Some(eoRef) =>
              //log.finest("D2WEditPage: render eo | inserted eos " + p.proxy.value.cache.insertedEOs)
              //log.finest("D2WEditPage: render eo | db eos " + p.proxy.value.cache.eos)

              val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value.cache, d2wContext.entityName.get, eoRef)
              log.finest("D2WEditPage: render eo out of cache: " + eoOpt)

              eoOpt match {
                case Some(eo) =>
                  val entityName = d2wContext.entityName.get
                  val ruleResults = p.proxy.value.ruleResults

                  log.finest("D2WEditPage: render check meta data fetched with d2wContext " + d2wContext)
                  log.finest("D2WEditPage: render check meta data fetched in rules " + ruleResults)

                  //if (metaDataPresent) {
                    log.finest("entityMetaDatas not empty")

                    //log.finest("Entity meta Data " + metaDatas)
                    val displayPropertyKeys = displayPropertyKeysFromProps(p, d2wContext)
                    val banImage = if (isEdit(p)) "/assets/images/EditBan.gif" else "/assets/images/InspectBan.gif"
                    val displayNameOpt = RuleUtils.ruleStringValueForContextAndKey(ruleResults, d2wContext, RuleKeys.displayNameForEntity)
                    val displayName = if (displayNameOpt.isDefined) displayNameOpt.get else ""

                    log.finest("Edit page EO " + eo)
                    <.div(
                      <.div(^.id := "b", MenuHeader(p.router, entityName, p.proxy)),
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
                              <.img(^.paddingRight := 11.px, ^.src := "/assets/images/ButtonCancel.gif", ^.onClick --> returnAction(p.router, entityName))
                            } else {
                              " "
                            },
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
                                        val updatedPageContext = pageContext.copy(d2wContext = propertyD2WContext)
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
                                            D2WComponentInstaller(p.router, updatedPageContext, p.proxy)
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
                  //} else {
                  //  <.div("no meta datas " + d2wContext)
                  //}
                case None => <.div("Object not found in cache")
              }
            case _ => <.div("Object Ref not found")
          }


        case None =>
          <.div("Show do something")

      }
    }
  }
  // create the React component for Dashboard
  private val component = ScalaComponent.builder[Props]("SignUpPage")
    .initialState(State(false))
    .renderBackend[Backend]
    //.componentWillMount(scope => scope.backend.willmounted(scope.props, scope.state))
    //.componentWillReceiveProps(scope => scope.backend.willReceiveProps(scope.currentProps, scope.nextProps, scope.state))
    //.componentWillUnmount(scope => scope.backend.clear)
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: PageContext, proxy: ModelProxy[MegaContent]) = {
    component(Props(ctl, d2wContext, proxy))
  }
}
