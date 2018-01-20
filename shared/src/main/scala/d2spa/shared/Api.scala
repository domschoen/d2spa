package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(entityName: String, queryValues: List[QueryValue]): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(entityName: String): Future[EntityMetaData]

  def newEO(entity: EOEntity, eo: EO): Future[EO]

  def updateEO(eo: EO): Future[EO]
  def deleteEO(eo: EO): Future[EO]
  def completeEO(eo: EO, missingKeys: Set[String]): Future[EO]
  def hydrateEORefs(eo: Seq[EORef], missingKeys: Set[String]): Future[Seq[EO]]
  def fireRules(rhs: D2WContext, keys: List[String]): Future[List[RuleResult]]
  def fireRule(rhs: D2WContext, key: String): Future[RuleResult]
  def fetchEOModel(): Future[EOModel]

}
