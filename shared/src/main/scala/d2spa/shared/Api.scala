package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(entity: String, queryValues: List[QueryValue]): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(entity: String): Future[EntityMetaData]

  def updateEO(entity: String, eo: EO): Future[EO]
  def deleteEO(eo: EO): Future[EO]

  def fireRules(d2WContext: D2WContext, keysToFire: List[String]): Future[List[RuleResult]]
}
