package d2spa.client.services

import d2spa.client.EOCacheUtils.{dbEntityMapForEntityNamed, eoCacheEntityElementForEntityNamed, refreshedEOMap, updatedCacheForDb}
import d2spa.client.RuleUtils.{missingKeysForKey, ruleContainerForContext, ruleResultForContextAndKey}
import d2spa.client.components.ERD2WEditToOneRelationship
import d2spa.client.{EOCache, RuleUtils, _}
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

  val testPageContext = PageContext(
    d2wContext = D2WContext(
      Some("CustomerBusiness"), // entity name
      Some(TaskDefine.inspect),
      None,
      Some("InspectEmbeddedCustomerBusinessManufacturer")
    ),
    eo = Some(EO("CustomerBusiness",
      List("businessUid", "customerUid"),
      List(IntValue(1),IntValue(30)),
      EOPk(List(1,303)), // pk
      None
    ))
    )

  val cacheBefore = Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) -> EO("Project",List(),List(),EOPk(List(-1)),None))))
  val cacheBefore2 = Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) -> EO("Project",List("descr"),List(StringValue("8")),EOPk(List(-1)),None))))

  def tests = TestSuite {
    "missingKeysForKey componentName should be empty" - {
      val ruleResults = d2spa.client.services.ClientTestData.ruleCache
      val d2wContext = D2WContext(Some("Project"),Some("edit"),None,None)
      val propertyKeys = List("descr", "projectNumber", "customer")
      val key = RuleKeys.componentName

      val propertyD2WContext = d2wContext.copy(propertyKey = Some("descr"))
      val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(ruleResults, propertyD2WContext, key)
      assert(ruleResultOpt.isDefined)



      val missingPropertyKeys = RuleUtils.missingKeysForKey(ruleResults, d2wContext, propertyKeys, key)
      assert(missingPropertyKeys.isEmpty)
    }
    "RuleRequest should be empty when rule cache is populated " - {
      val ruleResults = d2spa.client.services.ClientTestData.ruleCache
      val d2wContext = D2WContext(Some("Project"),Some("edit"),None,None)
      val displayPropertyKeysRuleResultPot = RuleUtils.potentialFireRuleResultPot(ruleResults, d2wContext, RuleKeys.displayPropertyKeys)
      val componentNameFireRulesOpt = RuleUtils.potentialFireRules(ruleResults, d2wContext, displayPropertyKeysRuleResultPot, RuleKeys.componentName)


      println("componentNameFireRulesOpt " + componentNameFireRulesOpt)
//componentNameFireRulesOpt Some(FireRules(List(descr, projectNumber, customer),componentName))
      val firingRulesReference = FireRules(List("descr", "projectNumber", "customer"), RuleKeys.componentName)
      assert(componentNameFireRulesOpt.isEmpty)


      val ruleRequest = RuleUtils.metaDataRuleRequest(ruleResults,d2wContext)
      assert(RulesUtilities.isEmptyRuleRequest(ruleRequest))
    }
    "Create new EO" - {
      val entityName = "Project"
      val newEO = EOCacheUtils.newEOWithLastMemID(entityName, None)
      val newEOReference = EO(entityName,
          List(),
          List(),
          EOPk(List(-1)),None)
      assert(newEO.equals(newEOReference))
    }
    "Create new EO and update cache" - {
      val cache = EOCache(Ready(SharedTestData.eomodel),Map(),Map())
      val entityName = "Project"

      val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(cache,entityName )

      val newEOReference = EO(entityName,
        List(),
        List(),
        EOPk(List(-1)),None)

      val newCacheReference = EOCache(Ready(SharedTestData.eomodel),
        // db eos
        Map(),
        // inserted eos
        Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) ->
          newEOReference
        )))
       )

      assert(newEO.equals(newEOReference))
      assert(newCache.equals(newCacheReference))
    }
    "Saved EO should be removed from mem cache and added to db cache" - {
      val entity = d2spa.shared.SharedTestData.projectEntity


      val beforeSaveInCacheEO = EO(entity.name,
        List("descr", "projectNumber"),
        List(StringValue("1"), IntValue(1)),
        EOPk(List(-1)),None)
      val savedEO = EO(entity.name,
        List("descr", "projectNumber", "id"),
        List(StringValue("1"), IntValue(1), IntValue(1)),
        EOPk(List(-1)),None)

      val cache = EOCache(Ready(SharedTestData.eomodel),
        // db eos
        Map("Customer" -> EODBCacheEntityElement(Ready(Map()))),
        // inserted eos
        Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) ->
          beforeSaveInCacheEO
          ))))
      val pk = EOValue.pk(SharedTestData.eomodel, savedEO)
      val updatedEO = savedEO.copy(pk = pk.get)

      val updatedEORef = EO(entity.name,
        List("descr", "projectNumber", "id"),
        List(StringValue("1"), IntValue(1), IntValue(1)),
        EOPk(List(1)),None)

      val newCache = EOCacheUtils.updatedCachesForSavedEO(cache, updatedEO, Some(savedEO))

      val newCacheReference = EOCache(Ready(SharedTestData.eomodel),
        // db eos
        Map("Customer" -> EODBCacheEntityElement(Ready(Map())),
          "Project" -> EODBCacheEntityElement(Ready(
            Map(EOPk(List(1)) -> updatedEORef)
          ))
        ),
        // inserted eos
        Map("Project" -> EOMemCacheEntityElement(Map(
        ))))

      assert(newCache.equals(newCacheReference))
    }
    'CacheInMemory - {
      val entity = d2spa.shared.SharedTestData.projectEntity
      val cache = EOCache(Ready(SharedTestData.eomodel),Map(),cacheBefore)
      val eosForUpdating = List(EO("Project",List("descr"),List(StringValue("1")),EOPk(List(-1)),None))
      val updatedCache = EOCacheUtils.updatedMemCacheWithEOsForEntityNamed(cache, eosForUpdating, "Project")
      assert(updatedCache.equals(
        EOCache(Ready(SharedTestData.eomodel),
          Map(),
          Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) -> EO("Project",List("descr"),List(StringValue("1")),EOPk(List(-1)),None)))))))
    }
    'CachePutNothingShouldReturnSome  - {
      val entity = d2spa.shared.SharedTestData.projectEntity

      // Empty cache
      val cache = EOCache(Ready(SharedTestData.eomodel),Map(),Map())
      val eos = List()
      val entityName = entity.name


      // Register an empty list for Project entity
      val newCache = EOCacheUtils.updatedDBCacheWithEOsForEntityNamed(cache, eos, entity.name)
      println("new Cache " + newCache)

      // Ask the cache for entity Project return a Pot which is Ready but with empty content
      val destinationEOs = EOCacheUtils.dbEOsForEntityNamed(newCache, entity.name)

      assert(destinationEOs.equals(Some(List())))
    }
    "Update cache for existing object" - {
      val eoCache = EOCache(Ready(SharedTestData.eomodel),Map(),Map())
      val eo = EO("Project",List(),List(),EOPk(List(1)))
      val entityName = eo.entityName
      val newCache = EOCacheUtils.updatedDBCacheWithEO(eoCache, eo)
      assert(newCache.eos.equals(Map("Project" -> EODBCacheEntityElement(Ready(Map(EOPk(List(1)) ->
        EO("Project",List(),List(),EOPk(List(1)),None)))))))
    }
    'Cache2 - {
      val entity = d2spa.shared.SharedTestData.projectEntity
      val eosForUpdating = List(EO("Project",List("descr","projectNumber"),List(StringValue("8"),IntValue(8)),EOPk(List(-1)),None))
      val entityMap = cacheBefore2("Project")

      val refreshedEOs = eosForUpdating.map(eo => {
        val pk = eo.pk
        Some((pk, eo))
      }).flatten.toMap
      val refreshedPks = refreshedEOs.keySet
      val existingPks = entityMap.data.keySet

      println("refreshedPks " + refreshedPks)
      println("existingPks " + existingPks)

      assert(refreshedPks.equals(Set(EOPk(List(-1)))))
      assert(existingPks.equals(Set(EOPk(List(-1)))))


      val cache = EOCache(Ready(SharedTestData.eomodel),Map(),cacheBefore2)
      val updatedCache = EOCacheUtils.updatedMemCacheWithEOsForEntityNamed(cache,eosForUpdating,"Project")
      println("updatedCache " + updatedCache)
      assert(updatedCache.equals(
        EOCache(Ready(SharedTestData.eomodel),
          Map(),
          Map("Project" -> EOMemCacheEntityElement(Map(EOPk(List(-1)) -> EO("Project",List("descr","projectNumber"),List(StringValue("8"),IntValue(8)),EOPk(List(-1)),None)))))))
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


      // testD2WContext
      val fireDisplayPropertyKeys = FireRule(RuleKeys.displayPropertyKeys)


      val fireDisplayPropertyKeysRuleResult = List(
        RuleResult(
          D2WContext(
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

    /* to restore 'FireRule - {
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
      }*/
      'GetRuleResult - {
        val h = build
        val testD2WContext = testPageContext.d2wContext
        val result = h.handle(SetJustRuleResults(fireDisplayPropertyKeysRuleResult))
        println("result " + result)

        // We want to test             val displayPropertyKeys = RuleUtils.ruleListValueForContextAndKey(newValue, AppModel.testD2WContext, RuleKeys.displayPropertyKeys)
        // Let's decompose it
        result match {
          case ModelUpdate(newValue) =>
            val pageConfOpt = RuleUtils.pageConfigurationRuleResultsForContext(newValue, testD2WContext)
            assert(pageConfOpt.isDefined)
            // println("pageConfOpt " + pageConfOpt)
            val ruleContainerOpt = RuleUtils.ruleContainerForContext(newValue, testD2WContext)
            println("ruleContainerOpt " + ruleContainerOpt)
            ruleContainerOpt match {
              case Some(rulesContainer) =>

                val ruleResultOpt = RuleUtils.ruleResultForContextAndKey(newValue, testD2WContext,RuleKeys.displayPropertyKeys)
                println("ruleResultOpt " + ruleResultOpt)
                ruleResultOpt match {
                  case Some(ruleResult) => {
                    println("rule result " + ruleResult)
                    // rule result RuleResult(D2WContextFullFledged(Some(CustomerBusiness),Some(inspect),None,Some(InspectEmbeddedCustomerBusinessManufacturer)),displayPropertyKeys,RuleValue(Some(comments),List()))
                    assert(ruleResult.value.stringV.isEmpty)
                    assert(ruleResult.value.stringsV.size == 1)

                    val displayPropertyKeys = RulesUtilities.ruleListValueWithRuleResult(ruleResultOpt)
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
      "Works with insertedEOs partialling filled" - {
        val exception = try {
          val cache = EOCache(Ready(SharedTestData.eomodel),Map(),Map())
          val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(cache, "Project")
          None
        } catch {
          case e: Throwable => Some(e)
          case _ => None
        }

        assert(exception.isEmpty)
      }
      "returns an updated cache" - {
        val cache = EOCache(Ready(SharedTestData.eomodel),Map(),Map())
        val (newCache, newEO) = EOCacheUtils.updatedMemCacheByCreatingNewEOForEntityNamed(cache, "Project")

        val insertedEOsForEntity = EOCacheUtils.allEOsForEntityNamed(newCache,"Project")
        println("insertedEOsForEntity " + insertedEOsForEntity)
        assert(insertedEOsForEntity.size == 1)

      }


    }
  }
}
