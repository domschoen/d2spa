package d2spa.client.services

import d2spa.client.RuleUtils.{ruleContainerForContext, ruleListValueWithRuleResult, ruleResultForContextAndKey}
import d2spa.client.components.ERD2WEditToOneRelationship
import d2spa.client.{RuleUtils, _}
import d2spa.client.services.RuleResultsHandler
import d2spa.shared
import diode.ActionResult._
import diode.RootModelRW
import diode.data._
import d2spa.shared._
import utest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object SPACircuitTests extends TestSuite {

  val testD2WContext = D2WContext(
    Some("CustomerBusiness"), // entity name
    Some(TaskDefine.inspect),
    None, // previousTask
    Some(EO("CustomerBusiness",
      List("businessUid", "customerUid"),
      List(IntValue(1),IntValue(30)),
      EOPk(List(1,303)), // pk
      None
    )),
    Map.empty[String, QueryValue], // queryValues
    None, // Data Rep
    None, // property Key
    PotFiredKey(Right(Some("InspectEmbeddedCustomerBusinessManufacturer"))),
    None)

  val cacheBefore = Map("Project" -> Map(EOPk(List(-1)) -> EO("Project",List(),List(),EOPk(List(-1)),None)))
  val cacheBefore2 = Map("Project" -> Map(EOPk(List(-1)) -> EO("Project",List("descr"),List(StringValue("8")),EOPk(List(-1)),None)))

  def tests = TestSuite {
    'Cache - {
        val eosForUpdating = List(EO("Project",List("descr"),List(StringValue("1")),EOPk(List(-1)),None))
        val updatedCache = EOCacheUtils.updatedModelForEntityNamed(SharedTestData.eomodel,cacheBefore,eosForUpdating)
        assert(updatedCache.equals(Map("Project" -> Map(EOPk(List(-1)) -> EO("Project",List("descr"),List(StringValue("1")),EOPk(List(-1)),None)))))
    }
    'Cache2 - {
      val eosForUpdating = List(EO("Project",List("descr","projectNumber"),List(StringValue("8"),IntValue(8)),EOPk(List(-1)),None))
      val entityMap = cacheBefore2("Project")

      val refreshedEOs = eosForUpdating.map(eo => {
        val pk = eo.pk
        Some((pk, eo))
      }).flatten.toMap
      val refreshedPks = refreshedEOs.keySet
      val existingPks = entityMap.keySet

      println("refreshedPks " + refreshedPks)
      println("existingPks " + existingPks)

      assert(refreshedPks.equals(Set(EOPk(List(-1)))))
      assert(existingPks.equals(Set(EOPk(List(-1)))))


      val updatedCache = EOCacheUtils.updatedModelForEntityNamed(SharedTestData.eomodel,cacheBefore2,eosForUpdating)
      println("updatedCache " + updatedCache)
      assert(updatedCache.equals(Map("Project" -> Map(EOPk(List(-1)) -> EO("Project",List("descr","projectNumber"),List(StringValue("8"),IntValue(8)),EOPk(List(-1)),None)))))
    }
    'EOValueCompleteEoWithEo - {
      val existingEO = EO("Project",List("descr"),List(StringValue("8")),EOPk(List(-1)),None)
      val refreshedEO = EO("Project",List("descr","projectNumber"),List(StringValue("8"),IntValue(8)),EOPk(List(-1)),None)
      val resultEO = shared.EOValue.completeEoWithEo(existingEO, refreshedEO)
      assert(resultEO.equals(refreshedEO))
    }
    'ERD2WEditToOneRelationship - {
      val eos = List(
        EO("Customer",
          List("acronym", "name", "id", "address", "type"),
          List(StringValue("2"), StringValue("2"), IntValue(2), StringValue("2"), StringValue("Customer")),
          EOPk(List(2)),
          None
        ),
        EO("Customer",
          List("acronym", "name", "id", "address", "type"),
          List(StringValue("1"), StringValue("1"), IntValue(1), StringValue("1"), StringValue("Customer")),
          EOPk(List(1)),
          None)
      )

      val eoOpt = ERD2WEditToOneRelationship.eoWith(eos, SharedTestData.customerEntity,"2")
      assert(eoOpt.isDefined)
    }



    'RuleResultsHandler - {
      val model = Map.empty[String, Map[String, Map[String, PageConfigurationRuleResults]]]



      val fireDisplayPropertyKeys = FireRule(testD2WContext, RuleKeys.displayPropertyKeys)


      val fireDisplayPropertyKeysRuleResult = List(
        RuleResult(
          D2WContextFullFledged(
            Some("CustomerBusiness"),
            Some(TaskDefine.inspect),
            None,
            Some("InspectEmbeddedCustomerBusinessManufacturer")
          ),
          RuleKeys.displayPropertyKeys,
          RuleValue(
            None,
            List("comments")
          )
        )
      )


      val customer = EO(
        d2spa.shared.SharedTestData.customerEntity.name,
        List("acronym","address","name","id"),
        List(StringValue("SFR"),StringValue("Rte de Paris 1"),StringValue("SFR"),IntValue(303)),
        EOPk(List(303)),
        None
      )



      def build = new RuleResultsHandler(new RootModelRW(model))

      'FireRule - {
        val h = build
        val result = h.handle(FireActions(testD2WContext, List(fireDisplayPropertyKeys)))
        result match {
          case EffectOnly(effect) =>
            effect.run {
              z => println("z " + z)
            }
            assert(effect.size == 1)
          case _ =>
            assert(false)
        }
      }
      'GetRuleResult - {
        val h = build
        val result = h.handle(SetRuleResults(fireDisplayPropertyKeysRuleResult, testD2WContext, List()))

        // We want to test             val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(newValue, AppModel.testD2WContext, RuleKeys.displayPropertyKeys)
        // Let's decompose it
        result match {
          case ModelUpdateEffect(newValue, effects) =>
            val pageConfOpt = RuleUtils.pageConfigurationRuleResultsForContext(newValue, testD2WContext)
            assert(pageConfOpt.isDefined)
            // println("pageConfOpt " + pageConfOpt)
            val ruleContainerOpt = RuleUtils.ruleContainerForContext(newValue, testD2WContext)
            ruleContainerOpt match {
              case Some(rulesContainer) =>
                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(rulesContainer.ruleResults, testD2WContext,RuleKeys.displayPropertyKeys)
                ruleResultOpt match {
                  case Some(ruleResult) => {
                    println("rule result " + ruleResult)
                    // rule result RuleResult(D2WContextFullFledged(Some(CustomerBusiness),Some(inspect),None,Some(InspectEmbeddedCustomerBusinessManufacturer)),displayPropertyKeys,RuleValue(Some(comments),List()))
                    assert(ruleResult.value.stringV.isEmpty)
                    assert(ruleResult.value.stringsV.size == 1)

                    val displayPropertyKeys = RuleUtils.ruleListValueWithRuleResult(ruleResultOpt)
                    assert(displayPropertyKeys.size == 1)
                    assert(displayPropertyKeys.equals(List("comments")))

                  }
                  case _ =>
                    assert(false)
                }
              case _ =>
                assert(false)
            }

          case _ =>
            assert(false)
        }
      }


    }
  }
}
