package d2spa.shared
import scala.concurrent.Future

trait Api {

  def search(entity: EOEntity, queryValues: List[QueryValue]): Future[Seq[EO]]

  def getMenus(): Future[Menus]

  def getMetaData(entityName: String): Future[EntityMetaData]

  def newEO(entity: EOEntity, eo: EO): Future[EO]

  def updateEO(eo: EO): Future[EO]
  def deleteEO(eo: EO): Future[EO]
  def completeEO(eo: EO, missingKeys: Set[String]): Future[EO]
  def hydrateEORefs(eo: List[EORef], missingKeys: Set[String]): Future[Seq[EO]]
  def fireRules(rules: Map[String,D2WContext]): Future[List[RuleResult]]
  def fireRule(rule: FireRule): Future[RuleResult]
  def fetchEOModel(): Future[EOModel]

}
