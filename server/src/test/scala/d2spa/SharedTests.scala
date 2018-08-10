package d2spa

import d2spa.shared._
import org.scalatestplus.play._

import scala.collection.mutable
import play.api.libs.json._
import services.ApiService

object TestData {
  val savingEO = EO("Project", List("descr", "projectNumber", "id"),
    List(
      StringValue("1"),
      IntValue(1),
      IntValue(5)
    ),
    EOPk(List(-1)),None)

  val insertedEOs = Map("Project" -> Map.empty[EOPk,EO])
  val emptyInsertedEOs = Map.empty[String,Map[EOPk,EO]]

}



class StackSpec extends PlaySpec {

  "Get pk" must {
    "entity is defined" in {
      val entityOpt = EOModelUtils.entityNamed(SharedTestData.eomodel,"Project")
      entityOpt.isDefined mustBe true
    }
    "gives EOPk with the value (5) extracted from the EO values" in {
      val pk = EOValue.pk(SharedTestData.eomodel, TestData.savingEO)
      pk.isDefined mustBe true
      val pkValues = pk.get.pks
      pkValues.size mustBe 1
      pkValues.head mustBe 5
    }
  }
  "Creation of a new EO" must {
    "Verification" in {
      val insertedEOsForEntity = TestData.insertedEOs("Project")
      val existingPks = insertedEOsForEntity.keySet.map(_.pks.head)
      println("existingPks " + existingPks)

      //val newMemID = existingPks.min - 1
      existingPks.size mustBe 0
    }
    "Works with insertedEOs partialling filled" in {
      val exception = try {
        val (newValue, newEO) = EOValue.createAndInsertNewObject(TestData.insertedEOs, "Project")
        None
      } catch {
        case e: Throwable => Some(e)
        case _ => None
      }

      exception.isEmpty mustBe true
    }
    "returns an updated cache" in {
      val (newValue, newEO) = EOValue.createAndInsertNewObject(TestData.emptyInsertedEOs, "Project")

      val insertedEOsForEntity = newValue("Project")
      println("insertedEOsForEntity " + insertedEOsForEntity)
      insertedEOsForEntity.size mustBe 1

    }
  }


}