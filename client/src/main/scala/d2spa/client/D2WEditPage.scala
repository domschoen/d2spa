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
import scala.collection.immutable.Set
import d2spa.client.logger._

import d2spa.client.SPAMain.{TaskAppPage}

object D2WEditPage {

  case class Props(router: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {

    def getTask(p: Props, entityName: String, taskName: String) = {
      val entityMetaData = p.proxy().entityMetaDatas.find(emd => emd.entity.name.equals(entityName)).get
      EntityMetaDataUtils.taskWithTaskName(entityMetaData,taskName)
    }

    // Page do a WillMount and components do a DidMount in order to have the page first (eo hydration has to be done first)
    def willmounted(p: Props) = {
      val entityName = p.d2wContext.entityName.get
      println("D2WEditPage: will Mount " + entityName)
      val taskName = p.d2wContext.task.get

      val entityMetaDataOpt = p.proxy().entityMetaDatas.find(emd => emd.entity.name.equals(entityName))
      val entityMetaDataNotFetched = entityMetaDataOpt.isEmpty
      println("entityMetaDataNotFetched " + entityMetaDataNotFetched)
      //val entity = props.proxy().menuModel.get.menus.flatMap(_.children).find(m => { m.entity.name.equals(props.entity) }).get.entity
      val fireDisplayPropertyKeys = FireRule(p.d2wContext, RuleKeys.displayPropertyKeys)

      println("D2WEditPage: eo " + p.eo)

      val actionList = if (p.eo.memID.isDefined) List(
        fireDisplayPropertyKeys,
        // in order to have an EO completed with all attributes for the task,
        // gives the eorefs needed for next action which is EOs for the eorefs according to embedded list display property keys
        Hydration(DrySubstrate(eo = Some(p.eo)),WateringScope(Some(FireRuleConverter.toRuleFault(fireDisplayPropertyKeys))))
      ) else List(fireDisplayPropertyKeys)


      Callback.when(true)(p.proxy.dispatchCB(
          FireActions(
            getTask(p,entityName,taskName),
            actionList
          )
      ))
    }

    def save(router: RouterCtl[TaskAppPage],entity: EOEntity,eo: EO) = {
      val isNewEO = EOValueUtils.isNew(eo)
      if (isNewEO) {
        Callback.log(s"Save new EO: $entity") >>
          $.props >>= (_.proxy.dispatchCB(NewEO(entity,eo)))
      } else {
        Callback.log(s"Save: $entity") >>
          $.props >>= (_.proxy.dispatchCB(Save(entity.name,eo)))
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

    def isEdit(p: Props) = p.d2wContext.task.get.equals(TaskDefine.edit)


    def entityMetaDataFromProps(p: Props): EntityMetaData =
      p.proxy.value.entityMetaDatas.find(emd => emd.entity.name.equals(p.d2wContext.entityName.get)).get

    def displayPropertyKeysFromProps(p: Props) = {
      val entityMetaData = entityMetaDataFromProps(p)
      val task = if (isEdit(p)) entityMetaData.editTask else entityMetaData.inspectTask
      task.displayPropertyKeys
    }

    def render(p: Props) = {
      val eoOpt = EOCacheUtils.outOfCacheEOUsingPkFromEO(p.proxy.value, p.eo)

      eoOpt match {
        case Some(eo) =>
          val entityName = p.d2wContext.entityName.get

          println("Render Edit page for entity: " + entityName + " and task " + p.d2wContext.task)
          val metaDatas = p.proxy.value
          val entityMetaDataNotFetched = p.proxy.value.entityMetaDatas.indexWhere(n => n.entity.name.equals(entityName)) < 0

          if  (!entityMetaDataNotFetched) {
            println("entityMetaDatas not empty")

            val entityMetaData = entityMetaDataFromProps(p)
            //log.debug("Entity meta Data " + metaDatas)
            val displayPropertyKeys = displayPropertyKeysFromProps(p)
            val entity = entityMetaData.entity
            val banImage = if (isEdit(p)) "/assets/images/EditBan.gif" else "/assets/images/InspectBan.gif"
            val task = if (isEdit(p)) entityMetaData.editTask else entityMetaData.inspectTask
            val eo = p.eo
            println("prox eo " + eo)

            println("Edit page EO " + eo)
            <.div(
              <.div(^.id:="b",MenuHeader(p.router,p.d2wContext.entityName.get,p.proxy)),
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
                    if (isEdit(p)) {
                      <.img(^.src := "/assets/images/ButtonSave.gif",^.onClick --> save(p.router,entity,eo))
                    } else {
                      " "
                    },
                    if (isEdit(p)) {
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
                              displayPropertyKeys toTagMod (property => {
                                val d2wContext = p.d2wContext.copy(propertyKey = Some(property.name))
                                <.tr(^.className := "attribute",
                                  <.th(^.className := "propertyName query", {
                                    val displayNameFound = RuleUtils.ruleStringValueForContextAndKey(property, d2wContext, RuleKeys.displayNameForProperty)
                                    val displayString = displayNameFound match {
                                      case Some(Some(stringValule)) => {
                                        //case Some(stringValule) => {
                                        stringValule
                                      }
                                      case _ => property.name
                                    }
                                    <.span(displayString)
                                  }),
                                  <.td(^.className := "query d2wAttributeValueCell",
                                    D2WComponentInstaller(p.router, d2wContext, property, eo, p.proxy)
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
            <.div("no meta datas")
          }
        case None => <.div("Object not found")
      }
    }
  }

  private val component = ScalaComponent.builder[Props]("D2WEditPage")
    .renderBackend[Backend]
    .componentWillMount(scope => scope.backend.willmounted(scope.props))
    .build

  def apply(ctl: RouterCtl[TaskAppPage], d2wContext: D2WContext, eo: EO, proxy: ModelProxy[MegaContent]) = {
    println("ctl " + ctl.hashCode())
    component(Props(ctl, d2wContext, eo, proxy))
  }
}
